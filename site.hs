{-# LANGUAGE OverloadedStrings #-}
module Main 
  where


import Prelude hiding (id)
import Control.Category (id)
import Control.Applicative
import Text.Pandoc
import Data.Monoid (mempty, mconcat, mappend, (<>))
import qualified Data.Map as M

import Hakyll
import Hakyll.Web.Pandoc

main :: IO ()
main = hakyllWith config $ do
  match "images/*" $ do
     route idRoute
     compile copyFileCompiler

  -- Compress CSS
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "tmp/index.html" $ do
    route idRoute
    compile $ getResourceBody >>= relativizeUrls

  -- Build tags
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  -- Render posts
  match "posts/*" $ do
    route $ setExtension ".html"
    compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions 
      >>= saveSnapshot "content"
      >>= return . fmap demoteHeaders
      >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
      >>= loadAndApplyTemplate "templates/default.html" (mathCtx `mappend` defaultContext)
      >>= relativizeUrls

  match "drafts/*" $ do
    route $ setExtension ".html"
    compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions 
      >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
      >>= loadAndApplyTemplate "templates/default.html" (mathCtx `mappend` defaultContext)
      >>= relativizeUrls

  -- Render posts list
  create ["posts.html"] $ do 
       route idRoute
       compile $ do
          list <- postList tags "posts/*" recentFirst
          makeItem ""
            >>= loadAndApplyTemplate "templates/posts.html"
                  (constField "title" "Posts" `mappend`
                   constField "posts" list    `mappend`
                   defaultContext)
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext)
            >>= relativizeUrls

  -- Post tags
  tagsRules tags $ \tag pattern -> do
    let title = "Post tagged " ++ tag

    route idRoute
    compile $ do
      list <- postList tags pattern recentFirst
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html"
               (constField "title" "Posts" `mappend`
               constField "posts" list    `mappend`
               mathCtx                    `mappend`
               defaultContext)
        >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext)
        >>= relativizeUrls
     -- Create RSS feed as well
    version "rss" $ do
        route $ setExtension "xml"
        compile $ loadAllSnapshots pattern "content"
          >>= return . take 10 . recentFirst
          >>= renderAtom (feedConfiguration title) feedCtx

    -- Index
  match "index.html" $ do
      route idRoute
      compile $ do
        list <- postList tags "posts/*" $ take 10 . recentFirst
        let indexContext =  constField "posts" list 
                         <> field "tags" (\_ -> renderTagList tags)
                         <> mathCtx
                         <> defaultContext
        getResourceBody
            >>= applyAsTemplate indexContext
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
      loadAllSnapshots "posts/*" "content"
        >>= return . take 10 . recentFirst
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

postList :: Tags -> Pattern -> ([Item String] -> [Item String]) -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts <- preprocess' <$> loadAll pattern
    applyTemplateList postItemTpl (postCtx tags) posts

config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave _site/* ../qnikst.github.com"
        }

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ if "mathjax" `M.member` metadata
                then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
                else ""


feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "Qnikst blog RSS feed - " ++ title
    , feedDescription = "here should be description.."
    , feedAuthorName  = "Alexander Vershilov"
    , feedAuthorEmail = "alexander.vershilov@gmail.com"
    , feedRoot        = "http://qnikst.github.com"
    }
