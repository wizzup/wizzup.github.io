--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.List
import           Data.Monoid         (mappend)
import           Hakyll
import           System.FilePath
import           Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "style-test.html" $ do
        route   idRoute
        compile $ getResourceString
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match "templates/*" $ compile templateBodyCompiler

    -- top-level pages :: /<page>/index.html
    match (fromList ["about.tex", "journey.rst", "contribution.tex"]) $ do
        route cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= cleanUrls
            >>= relativizeUrls

    let postFiles =    fromGlob "posts/*.md"
                  .||. fromGlob "posts/*.rst"
                  .||. fromGlob "posts/*.tex"

    -- blog posts :: /posts/<page>/index.html
    match postFiles $ do
        route cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= cleanUrls

    -- blog posts without templates :: /posts/<page>/raw.html
    match postFiles $ version "raw" $ do
        route cleanRouteRaw
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/raw.html"    postCtx
            >>= cleanUrls

    let draftFiles =   fromGlob "draft/*.md"
                  .||. fromGlob "draft/*.rst"
                  .||. fromGlob "draft/*.tex"

    -- draft blog posts :: /draft/<page>/index.html
    match draftFiles $ do
        route cleanRoute
        compile $ pandocCompiler
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

    create ["sitemap.xml"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
                let sitemapCtx = listField "entries" postCtx (return posts) `mappend`
                                 defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx


    match "index.tex" $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let indexCtx =
                    listField  "posts"  postCtx (return posts) `mappend`
                    constField "title"  "Home"                 `mappend`
                    defaultContext

            let readerOptions = defaultHakyllReaderOptions
            let writerOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" }

            pandocCompilerWith readerOptions writerOptions
                >>= loadAndApplyTemplate "templates/home.html"    indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= cleanUrls

--------------------------------------------------------------------------------
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
