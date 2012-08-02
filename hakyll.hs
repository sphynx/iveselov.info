{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Data.Monoid
import Hakyll
import System.FilePath

main :: IO ()
main = hakyllWith config $ do

  -- Copy images
    match "images/*" $ do
        route $ gsubRoute "^images" (const "static")
        compile copyFileCompiler

    -- Compress CSS
    match "css/*" $ do
        route $ gsubRoute "^css" (const "static")
        compile compressCssCompiler

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
        compile $ htmlPageCompiler
              >>> applyTemplateCompiler "templates/default.html"
              >>> relativizeUrlsCompiler

    -- CV in asciidoc, run "asciidoc" utility to produce CV
    match "cv.asciidoc" $ do
        route fancyUrlRoute
        compile $ getResourceString
          >>> unixFilter "asciidoc" ["-"]
          >>> arr readPage

    -- RST pages
    match "*.rst" $ do
        route fancyUrlRoute
        compile $ pageCompiler
              >>> applyTemplateCompiler "templates/default.html"
              >>> relativizeUrlsCompiler

    -- Pages index
    match "pages" $ route fancyUrlRoute
    create "pages" $ constA mempty
      >>> arr (setField "title" "Pages and experiments")
      >>> setFieldPageList recentFirst "templates/pageitem.html" "pages" "pages/*"
      >>> applyTemplateCompiler "templates/pages.html"
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

    -- xmonad article is shown as-is, without template decoration
    match "pages/xmonad.html" $ do
      route idRoute
      compile $ readPageCompiler >>> addDefaultFields

    -- Other html pages
    match "pages/*.html" $ do
      route idRoute
      compile $ htmlPageCompiler
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Read templates
    match "templates/*" $ compile templateCompiler


-- Transforms foo.html into foo/index.html to make possible '/foo'
-- links (instead of '/foo.html')
fancyUrlRoute :: Routes
fancyUrlRoute = customRoute $ (++ "/index.html") .
                dropExtension .
                identifierPath

-- Compiles HTML pages without invoking Pandoc on them
htmlPageCompiler :: Compiler Resource (Page String)
htmlPageCompiler =
   readPageCompiler >>>
   addDefaultFields >>>
   arr applySelf

-- Config
config :: HakyllConfiguration
config = defaultHakyllConfiguration
  { deployCommand =
       "rsync --checksum --progress -ave ssh _site/* " ++ to
  }
  where
    to = "sphynx@iveselov.info:iveselov.info/web"
