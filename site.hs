--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Data.Monoid          (mappend)
import           Control.Monad        (liftM)
import           Hakyll.Core.Compiler (unsafeCompiler)
import           KaTeX.KaTeXIPC       (kaTeXifyIO)

import qualified Data.Map             as M (fromList)
import qualified Data.Set             as S (fromList)

import           Text.Pandoc.Options  (writerExtensions)
import           Text.Pandoc.Extensions(Extension(Ext_latex_macros), enableExtension)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "pubs/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["main/papers.markdown", "main/cv.markdown"]) $ do
        route   $ gsubRoute "main/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "blog/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "posts"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "blog/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Blog Posts"          `mappend`
                    atomLinkCtx                              `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "blog/*" "posts"
            renderAtom feedConfig feedCtx posts

    match "shorts/*" $ do
      compile pandocCompiler

    pag <- buildPaginateLastPageWith (fmap (lazyPaginateThresholdEvery 5 3) . sortRecentFirst) "shorts/*" lazyPaginateFilePath

    paginateRules pag $ \ pageNum pat -> do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pat
        let paginateCtx = paginateContext pag pageNum
            ctx =
              listField "posts" postCtx (return posts) `mappend`
              constField "title" "Short Posts" `mappend`
              paginateCtx `mappend`
              defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "templates/shorts.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    match "main/index.html" $ do
        route $ gsubRoute "main/" (const "")
        compile $ do
            posts <- liftM (take 5) . recentFirst =<< loadAll "blog/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    atomLinkCtx `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
lazyPaginateEvery :: Int -> [a] -> [[a]]
lazyPaginateEvery n xs =
  let
    len = length xs
    overflow = len `mod` n
  in
    if overflow == 0 then
      paginateEvery n xs
    else
      let (first, rest) = splitAt (n + overflow) xs in
        first : paginateEvery n rest

lazyPaginateThresholdEvery :: Int -> Int -> [a] -> [[a]]
lazyPaginateThresholdEvery n t xs =
  let
    len = length xs
    overflow = len `mod` n
  in
    if overflow == 0 then
      paginateEvery n xs
    else
      let (first, rest) = splitAt (n + overflow) xs in
        if overflow >= t then
          take n first : drop n first : paginateEvery n rest
        else
          first : paginateEvery n rest

revLazyPaginateEvery :: Int -> [a] -> [[a]]
revLazyPaginateEvery n xs =
  reverse $ lazyPaginateEvery n xs

lazyPaginateFilePath :: PageNumber -> PageNumber -> Identifier
lazyPaginateFilePath last n =
  let revN = 1 + (last - n) in
  fromFilePath $ if revN == last then "void/index.html" else "void/void-" ++ (show revN) ++ ".html"

buildPaginateLastPageWith
  :: MonadMetadata m
  => ([Identifier] -> m [[Identifier]])
  -> Pattern
  -> (PageNumber -> PageNumber -> Identifier)
  -> m Paginate
buildPaginateLastPageWith grouper pat makeIdWithLastPage = do
  ids <- getMatches pat
  idGroups <- grouper ids
  let idSet = S.fromList ids
  return Paginate
    { paginateMap = M.fromList (zip [1 ..] idGroups)
    , paginateMakeId = makeIdWithLastPage (length idGroups)
    , paginateDependency = PatternDependency pat idSet
    }

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


atomLinkCtx :: Context String
atomLinkCtx =
    constField "atom-link" "/atom.xml" `mappend`
    constField "atom-title" (feedTitle feedConfig)


--------------------------------------------------------------------------------
pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler = do
  identifier <- getUnderlying
  s <- getMetadataField identifier "katex"
  case s of
    Just _ ->
      let
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        writerOptions = defaultHakyllWriterOptions
          { writerExtensions =
              enableExtension Ext_latex_macros defaultExtensions
          }
      in
      pandocCompilerWithTransformM defaultHakyllReaderOptions writerOptions
      (unsafeCompiler . kaTeXifyIO)
    Nothing -> pandocCompiler


--------------------------------------------------------------------------------
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "FEED TITLE"
  , feedDescription = "FEED DESCRIPTION"
  , feedAuthorName  = "AUTHOR NAME"
  , feedAuthorEmail = "author@example.com"
  , feedRoot        = "https://www.example.com"
  }
