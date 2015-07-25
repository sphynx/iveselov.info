{-# LANGUAGE OverloadedStrings #-}

module HakyllCmd where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid
import System.FilePath

import qualified Data.Map as M

import Hakyll

main :: IO ()
main = hakyllWith config $ do

    -- Copy images
    match "images/*" $ do
        route $ gsubRoute "^images" (const "static")
        compile copyFileCompiler

    -- Compress CSS
    match "css/*" $ do
        route $ gsubRoute "^css" (const "static")
        compile copyFileCompiler

    -- Setup favicon
    match "favicon.ico" $ do
        route idRoute
        compile copyFileCompiler

    -- Copy JS
    match "js/*.js" $ do
        route idRoute
        compile copyFileCompiler

    -- Index.html
    match "index.html" $ do
        route idRoute
        compile $ getResourceBody
          >>= applyAsTemplate lastPostContext
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    -- CV in asciidoc, run "asciidoc" utility to produce CV
    match "cv.asciidoc" $ do
        route fancyUrlRoute
        compile $ getResourceString >>= withItemBody (unixFilter "asciidoc" ["-"])

    -- Markdown pages
    match "*.md" $ do
        route fancyUrlRoute
        compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    -- Pages index
    create ["pages"] $ do
      route fancyUrlRoute
      compile $ do
        pages <- recentFirst =<< loadAll "pages/*"
        itemTpl <- loadBody "templates/pageitem.html"
        list <- applyTemplateList itemTpl defaultContext pages
        let pagesCtx =  constField "title" "Pages and experiments"
                     <> constField "pages" list
                     <> defaultContext
        makeItem list
          >>= loadAndApplyTemplate "templates/pages.html" pagesCtx
          >>= loadAndApplyTemplate "templates/default.html" pagesCtx
          >>= relativizeUrls

    -- xmonad article is shown as-is, without template decoration
    match "pages/xmonad.html" $ do
      route idRoute
      compile getResourceBody

    -- Other html pages
    match "pages/*.html" $ do
      route idRoute
      compile $ htmlPageCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    -- Posts
    match "posts/*" $ do
      route $ setExtension ".html"
      compile $ pandocCompiler
         >>= loadAndApplyTemplate "templates/post.html" defaultContext
         >>= loadAndApplyTemplate "templates/default.html" defaultContext
         >>= relativizeUrls

    -- Render posts list
    create ["posts"] $ do
      route fancyUrlRoute
      compile $ do
        pages <- recentFirst =<< loadAll "posts/*"
        itemTpl <- loadBody "templates/postitem.html"
        list <- applyTemplateList itemTpl defaultContext pages
        let postsCtx =  constField "title" "Blog posts"
                     <> constField "posts" list
                     <> defaultContext
        makeItem list
          >>= loadAndApplyTemplate "templates/posts.html" postsCtx
          >>= loadAndApplyTemplate "templates/default.html" postsCtx
          >>= relativizeUrls

    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Render RSS feed
    create ["rss.xml"] $ do
      route idRoute
      compile $ loadAll "posts/*"
        >>= renderRss feedConfiguration defaultContext

-- Transforms foo.html into foo/index.html to make possible '/foo'
-- links (instead of '/foo.html')
fancyUrlRoute :: Routes
fancyUrlRoute = customRoute $
    (++ "/index.html")
  . dropExtension
  . toFilePath

-- Compiles HTML pages without invoking Pandoc on them
htmlPageCompiler :: Compiler (Item String)
htmlPageCompiler = getResourceBody >>= applyAsTemplate defaultContext

-- Config
config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync --checksum --progress -ave ssh _site/* " ++ to
  } where to = "sphynx@iveselov.info:/srv/iveselov.info"

-- Feed config
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Ivan Veselov - a personal blog"
    , feedDescription = "Personal blog of Ivan Veselov"
    , feedAuthorName  = "Ivan N. Veselov"
    , feedAuthorEmail = "veselov@gmail.com"
    , feedRoot        = "http://iveselov.info"
    }

lastPostContext :: Context a
lastPostContext = Context $ \key _ -> case key of
  "lastposttitle" -> lastPostTitle
  "lastposturl"   -> lastPostUrl
  _               -> empty
  where

    lastPost :: Compiler Identifier
    lastPost = do
      postIds <- (reverse . sort) <$> getMatches "posts/*"
      case postIds of
        [] -> empty
        lastPostId : _ -> return lastPostId

    lastPostTitle :: Compiler ContextField
    lastPostTitle = do
      metadata <- getMetadata =<< lastPost
      return $ StringField $ fromMaybe empty $ M.lookup "title" metadata

    lastPostUrl :: Compiler ContextField
    lastPostUrl = do
      p <- lastPost
      (StringField . maybe empty toUrl) <$> getRoute p

