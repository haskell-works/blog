{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Picture             (writeDynamicPng)
import Data.Monoid               ((<>))
import Hakyll
import Hakyll.Contrib.LaTeX
import Hakyll.Web.Pandoc
import Image.LaTeX.Render        (defaultEnv, displaymath, imageForFormula)
import Image.LaTeX.Render.Pandoc

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}


main :: IO ()
main = do
  renderFormulae <- initFormulaCompilerDataURI 1000 defaultEnv

  hakyll $ do
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
      route   $ setExtension "html"
      compile $ pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions (renderFormulae defaultPandocFormulaOptions)
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    match "posts/*" $ do
      route $ setExtension "html"
      compile $ pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions (renderFormulae defaultPandocFormulaOptions)
        >>= saveSnapshot "post-content"
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

    match "drafts/*" $ do
      route $ setExtension "html"
      compile $ pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions (renderFormulae defaultPandocFormulaOptions)
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
              listField "posts" postCtx (return posts) <>
              constField "title" "Archives"            <>
              defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    create ["drafts.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "drafts/*"
        let archiveCtx =
              listField "posts" postCtx (return posts) <>
              constField "title" "Drafts"              <>
              defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend`
              constField "title" "Home"                `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    -- http://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
    let
      rss name render' =
        create [name] $ do
          route idRoute
          compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/**" "post-content"
            render' feedConfiguration feedCtx posts

    rss "rss.xml" renderRss
    rss "atom.xml" renderAtom

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration {
      feedTitle       = "Haskell Works Blog"
    , feedDescription = ""
    , feedAuthorName  = "Haskell Works"
    , feedAuthorEmail = ""
    , feedRoot        = "https://haskell-works.github.io"
    }
