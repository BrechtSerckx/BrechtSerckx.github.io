module Main where

import           Hakyll
import           Hakyll.Web.Sass                ( sassCompiler )
import           Data.List                      ( sortOn )
import           System.FilePath                ( (</>)
                                                , addExtension
                                                )
import           Hakyll.Web.Template.Context.Path
                                                ( metadataPathFrom )
import           Hakyll.Web.Template.Context.Yaml
                                                ( yamlMetadataPathFrom )
import           Hakyll.Web.Pandoc.Metadata     ( readMetadataValueWith )
import           Text.Pandoc.Options            ( readerExtensions
                                                , githubMarkdownExtensions
                                                , ReaderOptions
                                                , WriterOptions
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

  match "css/*.scss" $ do
    route $ setExtension "css"
    compile $ fmap compressCss <$> sassCompiler

  match "data/**.md" . compile $ pandocCompilerWith readerOptions writerOptions

  create ["index", "about"]           pageRule
  create ["professional", "projects"] pageRuleWithSubdir

  match "templates/*" $ compile templateBodyCompiler

pageRule :: Rules ()
pageRule = pageRuleWith . const $ pure mempty

pageRuleWithSubdir :: Rules ()
pageRuleWithSubdir = pageRuleWith $ \id' -> do
  let dataDir = fromGlob $ "data" </> toFilePath id' </> "*"
  items <- sortOn itemIdentifier <$> loadAll dataDir
  pure $ listField "items" defaultContext (pure items)

pageRuleWith :: (Identifier -> Compiler (Context String)) -> Rules ()
pageRuleWith mkExtraCtx = do
    -- Route to `<id>.html`
  route . customRoute $ \id' -> addExtension (toFilePath id') "html"

  -- Compile, filling in the data from `data/<id>.md` in the nested templates.
  -- If a subdirectory `data/<id>/` exists, make its contents available in
  -- `items` list.
  compile $ do
    id' <- getUnderlying
    let
      routeFile = fromFilePath $ "data" </> addExtension (toFilePath id') "md"
      routeTemplate =
        fromFilePath $ "templates" </> addExtension (toFilePath id') "html"
      bodyCtx     = field "body" $ \_ -> loadBody routeFile
      metadataCtx = metadataPathFrom
        (readMetadataValueWith readerOptions writerOptions)
        routeFile
      templateCtx = metadataPathFrom
        (readMetadataValueWith readerOptions writerOptions)
        routeTemplate
      settingsCtx = yamlMetadataPathFrom
        (readMetadataValueWith readerOptions writerOptions)
        "data/default.yaml"
    extraCtx <- mkExtraCtx id'
    makeItem ""
      >>= loadAndApplyTemplate
            routeTemplate
            (bodyCtx <> extraCtx <> metadataCtx <> settingsCtx <> defaultContext
            )
      >>= loadAndApplyTemplate
            "templates/default.html"
            (templateCtx <> metadataCtx <> settingsCtx <> defaultContext)
      >>= relativizeUrls

readerOptions :: ReaderOptions
readerOptions =
  defaultHakyllReaderOptions { readerExtensions = githubMarkdownExtensions }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions
