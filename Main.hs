{-# LANGUAGE OverloadedStrings #-}
module Main 
  where

import Prelude hiding (id)
import Control.Category (id)
import Control.Applicative
import Text.Pandoc
import Data.Monoid (mempty, mconcat, mappend, (<>))
import qualified Hakyll.Core.Metadata as Meta
import qualified Data.Map as M

import Hakyll
--import Hakyll.Web.Pandoc
import Data.Char

import Debug.Trace

main :: IO ()
main = hakyllWith config $ do

  match "img/**" $ do
     route idRoute
     compile copyFileCompiler
  match "images/**" $ do
     route idRoute
     compile copyFileCompiler

  match "fonts/*" $ do
     route idRoute
     compile copyFileCompiler

  match "css/*" $ compile compressCssCompiler
  create ["style.css"] $ do
     route idRoute
     compile $ do
       csss <- loadAll "css/*.css"
       makeItem $ unlines $ map itemBody csss

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  -- Build tags
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  -- Render posts
  match "posts/*" $ do
    route $ setExtension ".html"
    compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions 
      >>= saveSnapshot "content"
      >>= return . fmap demoteHeaders
      >>= loadAndApplyTemplate "templates/post.html" (licenseCtx <> postCtx tags <> keywordCtx)
      >>= loadAndApplyTemplate "templates/default.html" (mathCtx  <> defaultContext)
      >>= relativizeUrls

  match "posts/*.lhs" $ version "raw" $ do
    route idRoute
    compile getResourceBody

  match "drafts/*" $ do
    route $ setExtension ".html"
    compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions 
      >>= loadAndApplyTemplate "templates/post.html" (postCtx tags <> keywordCtx)
      >>= loadAndApplyTemplate "templates/default.html" (mathCtx `mappend` defaultContext)
      >>= relativizeUrls

  -- Render posts list
  create ["posts.html"] $ do 
       route idRoute
       compile $ do
          let ctx =  constField "title" "Posts"
                  <> listField "posts" -- defaultContext
                       (teaserField "teaser" "content" <> defaultContext)
                       (recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion))
                  <> field "tags" (\_ -> renderTagList tags)
                  <> defaultContext
          makeItem ""
            >>= loadAndApplyTemplate "templates/posts.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext)
            >>= relativizeUrls

  -- Post tags
  tagsRules tags $ \tag pattern -> do
    let title = "Post tagged " ++ tag

    route idRoute
    compile $ do
      let ctx = 
               (constField "title" "Posts" `mappend`
                  listField "posts" (teaserField "teaser" "content" <> defaultContext)
                  (recentFirst =<< loadAll (pattern .&&. hasNoVersion))  `mappend`
               field "tags" (\_ -> renderTagList tags) `mappend`
               mathCtx                    `mappend`
               licenseCtx                 `mappend`
               defaultContext)
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext)
        >>= relativizeUrls
     -- Create RSS feed as well
    version "rss" $ do
        route $ setExtension "xml"
        compile $ loadAllSnapshots (pattern .&&. hasNoVersion) "content"
          >>= fmap (take 10) . recentFirst
          >>= renderAtom (feedConfiguration title) feedCtx

    -- Index
  create ["index.html"] $ do
      route idRoute
      compile $ do
        let indexContext =  listField "posts" defaultContext (fmap (take 10) . recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion))
                         <> field "tags" (\_ -> renderTagList tags)
                         <> mathCtx
                         <> defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "templates/index.html" indexContext
            >>= loadAndApplyTemplate "templates/default.html" indexContext
            >>= relativizeUrls


  -- Render some static pages
  match (fromList ["projects.html","contact.html"]) $ do
    route idRoute
    compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" 
                (mathCtx `mappend` defaultContext)
        >>= relativizeUrls

  -- Render RSS feed
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
        >>= fmap (take 10) . recentFirst
        >>= renderAtom (feedConfiguration "All posts") feedCtx

  match "templates/*" $ compile $ templateCompiler


postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave _site/* ../qnikst.github.com"
        }

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ case "mathjax" `Meta.lookupString` metadata of
               Just{} -> "<script type=\"text/javascript\" src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
               Nothing -> ""

keywordCtx :: Context String
keywordCtx = field "metaKeywords" $ \item -> do
    tags <- getMetadataField (itemIdentifier item) "tags"
    return $ maybe "" showMetaTags tags
  where
    showMetaTags t = "<meta name=\"keywords\" content=\""++t++"\">\n"

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "Qnikst blog RSS feed - " ++ title
    , feedDescription = "qnikst blog: gentoo, haskell, etc."
    , feedAuthorName  = "Alexander Vershilov"
    , feedAuthorEmail = "alexander.vershilov@gmail.com"
    , feedRoot        = "https://qnikst.github.io"
    }

licenseCtx :: Context a
licenseCtx = field "license" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ case Meta.lookupString "license" metadata of
               Nothing -> ""
               Just m -> case M.lookup (trim m) licenses of
                           Nothing -> "unknown license"
                           Just (u,i) -> "<a href=\""++u++"\"><img src=\""++i++"\"/></a>"
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

--licenses :: String -> String
licenses = M.fromList 
    [ ("by",       ( "https://creativecommons.org/licenses/by/3.0"
                   , "https://i.creativecommons.org/l/by/3.0/88x31.png"))
    , ("by-sa",    ( "https://creativecommons.org/licenses/by-sa/3.0"
                   , "https://i.creativecommons.org/l/by-sa/3.0/88x31.png"))
    , ("by-nd",    ( "https://creativecommons.org/licenses/by/3.0"
                   , "https://i.creativecommons.org/l/by/3.0/88x31.png"))
    , ("by-nc",    ( "https://creativecommons.org/licenses/by/3.0"
                   , "https://i.creativecommons.org/l/by/3.0/88x31.png"))
    , ("by-nc-sa", ( "https://creativecommons.org/licenses/by-nc-sa/3.0"
                   , "https://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png"))
    , ("by-nc-nd", ( "https://creativecommons.org/licenses/by-nc-nd/3.0"
                   , "https://i.creativecommons.org/l/by-nc-nd/3.0/88x31.png"))]

