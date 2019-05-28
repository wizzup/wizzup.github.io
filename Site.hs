--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List           (isSuffixOf)
import Data.Monoid         (mappend)
import Hakyll
import System.FilePath
import Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    -- static files
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        -- compile compressCssCompiler
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- search engine files
    match (fromList ["robots.txt", "google9b5d7c8afd9c3a00.html"]) $ do
        route   idRoute
        compile copyFileCompiler

    -- testing files
    match "style-test.html" $ do
        route   idRoute
        compile $ getResourceString
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    -- templates
    match "templates/*" $ compile templateBodyCompiler

    -- top-level pages :: me/<page>/index.html
    let meFiles = listFiles "me"
    -- top-level pages :: /<page>/index.html
    let topFiles = fromList ["about.tex", "journey.rst", "contribution.tex"]
    match (topFiles .||. meFiles) $ do
        route cleanRoute
        compile $ pandocCompilerWith pandocReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= cleanUrls
            >>= relativizeUrls


    -- blog posts :: /posts/<page>/index.html
    let postFiles = listFiles "posts"
    match postFiles $ do
        route cleanRoute
        compile $ pandocCompilerWith pandocReaderOptions pandocWriterOptions
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    (teaserField "teaser" "content" <> postCtx)
            >>= loadAndApplyTemplate "templates/default.html" (teaserField "teaser" "content" <> postCtx)
            >>= cleanUrls

    -- -- blog posts without templates :: /posts/<page>/raw.html
    -- match postFiles $ version "raw" $ do
    --     route cleanRouteRaw
    --     compile $ pandocCompilerWith pandocReaderOptions pandocWriterOptions
    --         >>= loadAndApplyTemplate "templates/raw.html"    postCtx
    --         >>= cleanUrls

    let draftFiles = listFiles "draft"

    -- draft blog posts :: /draft/<page>/index.html
    match draftFiles $ do
        route cleanRoute
        compile $ pandocCompilerWith pandocReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= cleanUrls

    -- top-level pages :: /draft/index.html
    create ["draft.html"] $ do
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll "draft/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Draft"               `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= cleanUrls

    -- top-level pages :: /archive/index.html
    create ["archive.html"] $ do
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= cleanUrls

    -- FIXME: add top-level and me to sitemap
    create ["sitemap.xml"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
                let sitemapCtx = listField "entries" postCtx (return posts) `mappend`
                                 defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    -- /index.html
    match "index.tex" $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let indexCtx =
                    listField  "posts"  postCtx (return posts) `mappend`
                    constField "title"  "Home"                 `mappend`
                    defaultContext

            pandocCompilerWith pandocReaderOptions pandocWriterOptions
                >>= loadAndApplyTemplate "templates/home.html"    indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= cleanUrls

--------------------------------------------------------------------------------
-- readerOptions = defaultHakyllReaderOptions
--
-- writerOptions = defaultHakyllWriterOptions
--                   { writerHTMLMathMethod = MathJax "" }

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = def { readerExtensions = foldr enableExtension
                            githubMarkdownExtensions [
                              Ext_fenced_code_attributes
                            , Ext_raw_attribute
                            ]
                          }

pandocWriterOptions :: WriterOptions
pandocWriterOptions = def
  {
    -- writerHtml5          = True,
    -- writerHighlight      = True,
    writerHTMLMathMethod = MathML,
    writerReferenceLinks = True
  }

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    dateField "dateNum" "%F" `mappend`
    defaultContext

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                            where p = toFilePath ident

cleanRouteRaw :: Routes
cleanRouteRaw = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "raw.html"
                            where p = toFilePath ident

cleanUrls :: Item String -> Compiler (Item String)
cleanUrls x =   relativizeUrls x
            >>= cleanIndexUrls
            >>= cleanIndexHtmls

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll "/index.html" replacement)
  where
    replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

listFiles :: String -> Pattern
listFiles dir = fromGlob (dir ++ "/*.md")
           .||. fromGlob (dir ++ "/*.rst")
           .||. fromGlob (dir ++ "/*.tex")
           -- .||. fromGlob (dir ++ "/*.txt")
