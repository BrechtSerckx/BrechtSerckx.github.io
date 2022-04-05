module Main where

import           Data.List                      ( sortOn )
import qualified Data.Yaml                     as Yaml
import           Hakyll
import           Hakyll.Web.Sass                ( sassCompiler )
import           Hakyll.Web.Template.Context.Path
                                                ( metadataContext )
import           System.FilePath                ( (</>)
                                                , addExtension
                                                )
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

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*.scss" $ do
    route $ setExtension "css"
    compile $ fmap compressCss <$> sassCompiler

  match "data/**.md" . compile $ pandocCompilerWith readerOptions writerOptions

  create ["index", "about"]           pageRule
  create ["professional", "projects"] pageRuleWithSubdir

  match "templates/**" $ compile templateBodyCompiler

pageRule :: Rules ()
pageRule = pageRuleWith . const $ pure mempty

pageRuleWithSubdir :: Rules ()
pageRuleWithSubdir = pageRuleWith $ \id' -> do
  let dataDir = fromGlob $ "data" </> toFilePath id' </> "*"
  items <- sortOn itemIdentifier <$> loadAll dataDir
  pure $ \ctx -> listField "items" ctx (pure items)

pageRuleWith
  :: (Identifier -> Compiler (Context String -> Context String)) -> Rules ()
pageRuleWith mkExtraCtx = do
    -- Route to `<id>.html`
  route . customRoute $ \id' -> addExtension (toFilePath id') "html"

  -- Compile, filling in the data from `data/<id>.md` in the nested templates.
  -- If a subdirectory `data/<id>/` exists, make its contents available in
  -- `items` list.
  compile $ do
    id' <- getUnderlying
    let routeFile =
          fromFilePath $ "data" </> addExtension (toFilePath id') "md"
        routeTemplate =
          fromFilePath $ "templates" </> addExtension (toFilePath id') "html"
        bodyCtx        = field "body" $ \_ -> loadBody routeFile
        defaultBodyCtx = bodyField "body"
        defaultUrlCtx  = urlField "url"
    pageMetadata     <- getMetadata routeFile
    templateMetadata <- getMetadata routeTemplate
    settings <- unsafeCompiler $ Yaml.decodeFileThrow "data/default.yaml"
    extraCtx         <- mkExtraCtx id'
    makeItem ""
      >>= loadAndApplyTemplate
            routeTemplate
            (mkCtx $ \ctx ->
              bodyCtx
                <> extraCtx (defaultBodyCtx <> metadataField <> ctx)
                <> metadataContext (Just pageMetadata) ctx
                <> metadataContext (Just settings)     ctx
                <> defaultUrlCtx
            )
      >>= loadAndApplyTemplate
            "templates/default.html"
            (mkCtx $ \ctx ->
              metadataContext (Just templateMetadata) ctx
                <> metadataContext (Just pageMetadata) ctx
                <> metadataContext (Just settings)     ctx
                <> defaultBodyCtx
                <> defaultUrlCtx
            )
      >>= relativizeUrls

mkCtx :: (Context a -> Context a) -> Context a
mkCtx withCtx = withCtx $ mkCtx withCtx

readerOptions :: ReaderOptions
readerOptions =
  defaultHakyllReaderOptions { readerExtensions = githubMarkdownExtensions }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions
