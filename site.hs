{-# LANGUAGE OverloadedStrings #-}
module Main 
  where


import Prelude hiding (id)
import Control.Category (id)
import Control.Applicative
import Data.Monoid (mempty, mconcat, mappend)

import Hakyll

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
     route idRoute
     compile copyFileCompiler

  -- Compress CSS
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "tmp/index.html" $ do
    route idRoute
    compile $ getResourceBody >>= relativizeUrls

  -- Build tags
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  -- Render posts
  match "posts/*" $ do
    route $ setExtension ".html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= return . fmap demoteHeaders
      >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
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
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
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
               defaultContext)
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    -- Index
  match "index.html" $ do
      route idRoute
      compile $ do
        list <- postList tags "posts/*" $ take 3 . recentFirst
        let indexContext = constField "posts" list `mappend`
                           field "tags" (\_ -> renderTagList tags) `mappend`
                           defaultContext
        getResourceBody
            >>= applyAsTemplate indexContext
            >>= loadAndApplyTemplate "templates/default.html" indexContext
            >>= relativizeUrls


  -- Render some static pages
  match (fromList ["projects.rst","contact.markdown"]) $ do
    route $ setExtension ".html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "templates/*" $ compile $ templateCompiler


postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

postList :: Tags -> Pattern -> ([Item String] -> [Item String]) -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts <- preprocess' <$> loadAll pattern
    applyTemplateList postItemTpl (postCtx tags) posts




{-
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "SimpleBlog RSS feed."
    , feedDescription = "A simple demo of an RSS feed created with Hakyll."
    , feedAuthorName  = "Jasper Van der Jeugt"
    , feedAuthorEmail = "test@example.com"
    , feedRoot        = "http://example.com"
    }
-}
