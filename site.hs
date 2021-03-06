module Main where

import qualified Data.Yaml                     as Yaml
import           Data.Function                  ( fix )
import qualified Data.Aeson.Types              as Aeson
import           Data.Foldable                  ( fold
                                                , asum
                                                )
import           Hakyll
import           Control.Monad                  ( (>=>) )
import           Hakyll.Web.Pandoc.Metadata
import           Hakyll.Web.Sass                ( sassCompiler )
import           Hakyll.Web.Template.Context.Path
                                                ( mkMetadataContext
                                                , defaultParseContextField
                                                )
import qualified System.FilePath               as FP
import           Text.Pandoc.Options            ( ReaderOptions
                                                , WriterOptions
                                                , githubMarkdownExtensions
                                                , readerExtensions
                                                )

main :: IO ()
main = hakyll siteRules

mainBuild :: IO ()
mainBuild =
  let configuration = defaultConfiguration
      options       = Options False Build
  in  hakyllWithArgs configuration options siteRules

siteRules :: Rules ()
siteRules = do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "images/**.svg" $ compile templateBodyCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*.scss" $ do
    route $ setExtension "css"
    compile $ fmap compressCss <$> sassCompiler

  match "html/**.template.html" $ compile templateBodyCompiler

  match "html/**.page.*" $ do
      -- Route to `<id>.html`
    route
      . customRoute
      $ flip FP.replaceExtension "html"
      . FP.dropExtension
      . FP.joinPath
      . tail
      . FP.splitPath
      . toFilePath

    -- Compile, filling in the data from `data/<id>.md` in the nested templates.
    -- If a subdirectory `data/<id>/` exists, make its contents available in
    -- `items` list.
    compile $ do
      id' <- getUnderlying
      let go id'' = do
            getMetadataField id'' "template" >>= \case
              Nothing           -> pure mempty
              Just templatePath -> do
                let templateId = fromFilePath templatePath
                templateMetadata <- getMetadata templateId
                ((templateId, templateMetadata) :) <$> go templateId
      templates    <- go id'
      pageMetadata <- getMetadata id'
      settings     <- unsafeCompiler $ Yaml.decodeFileThrow "settings.yaml"
      let
        pageCtx = mkCtx $ \ctx ->
          bodyField "body"
            <> metadataContext (Just pageMetadata) ctx
            <> fold (flip metadataContext ctx . Just . snd <$> templates)
            <> metadataContext (Just settings) ctx
        loadAndApplyWithMetadata [] = pure
        loadAndApplyWithMetadata ((templateId, templateMetadata) : rest) =
          let
            templateCtx = mkCtx $ \ctx ->
              bodyField "body"
                <> metadataContext (Just pageMetadata)     ctx
                <> metadataContext (Just templateMetadata) ctx
                <> fold (flip metadataContext ctx . Just . snd <$> templates)
                <> metadataContext (Just settings) ctx
          in  loadAndApplyTemplate templateId templateCtx
                >=> loadAndApplyWithMetadata rest
      getResourceBody
        >>= applyAsTemplate pageCtx
        >>= loadAndApplyWithMetadata templates
        >>= relativizeUrls

mkCtx :: (Context a -> Context a) -> Context a
mkCtx withCtx = withCtx $ mkCtx withCtx

readerOptions :: ReaderOptions
readerOptions =
  defaultHakyllReaderOptions { readerExtensions = githubMarkdownExtensions }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions

metadataContext :: Maybe Yaml.Object -> Context String -> Context String
metadataContext mI ctx = mkMetadataContext mI . fix $ \f v ->
  let pandocParseContextField = case Aeson.fromJSON v of
        Aeson.Error   _  -> pure Nothing
        Aeson.Success mv -> Just <$> readMetadataValue mv
  in  asum
        <$> sequence [pandocParseContextField, defaultParseContextField ctx f v]
