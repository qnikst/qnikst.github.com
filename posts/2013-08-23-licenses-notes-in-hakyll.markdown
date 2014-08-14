---
author: Alexander Vershilov
date: 2013-08-26
title: Adding license notes to blog pages.
license: by-nc-sa
tags: hakyll, haskell
---

I've finally manged to create configurable license notes for each article:

To do it you need to add additional context:


    licenseCtx :: Context a
    licenseCtx = field "license" $ \item -> do
        metadata <- getMetadata $ itemIdentifier item
        return $ case M.lookup "license" metadata of
                        Nothing -> ""
                        Just m -> case M.lookup (trim m) licenses of
                                          Nothing -> "unknown license"
                                          Just (u,i) -> "<a href=\""++u++"\"><img src=\""++i++"\"/></a>"
      where
        trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace


    licenses = m.fromlist 
        [ ("by",       ( "http://creativecommons.org/licenses/by/3.0"
                       , "http://i.creativecommons.org/l/by/3.0/88x31.png"))
        , ("by-sa",    ( "http://creativecommons.org/licenses/by-sa/3.0"
                       , "http://i.creativecommons.org/l/by-sa/3.0/88x31.png"))
        , ("by-nd",    ( "http://creativecommons.org/licenses/by/3.0"
                       , "http://i.creativecommons.org/l/by/3.0/88x31.png"))
        , ("by-nc",    ( "http://creativecommons.org/licenses/by/3.0"
                       , "http://i.creativecommons.org/l/by/3.0/88x31.png"))
        , ("by-nc-sa", ( "http://creativecommons.org/licenses/by-nc-sa/3.0"
                       , "http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png"))
        , ("by-nc-nd", ( "http://creativecommons.org/licenses/by-nc-nd/3.0"
                       , "http://i.creativecommons.org/l/by-nc-nd/3.0/88x31.png"))]

Currently only cc-* licenses are added, and but it will be possible to add another.

To apply license to template just add `$license$` and add context:

      loadAndApplyTemplate "templates/post.html" (licenseCtx <> postCtx tags)


For my blog I'm trying to use:

  * by - for announces and physics
  * by-nc-sa - for documentation like posts
  * by-nc-nd - for personal posts
