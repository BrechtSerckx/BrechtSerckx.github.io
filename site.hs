module Main where

import           Hakyll
import           Data.List                      ( sortOn )
import           System.FilePath                ( (</>)
                                                , addExtension
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

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "data/**.md" $ compile pandocCompiler

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
    let routeFile =
          fromFilePath $ "data" </> addExtension (toFilePath id') "md"
        routeTemplate =
          fromFilePath $ "templates" </> addExtension (toFilePath id') "html"
        bodyCtx     = field "body" $ \_ -> loadBody routeFile
        metadataCtx = metadataFieldFrom routeFile
        templateCtx = metadataFieldFrom routeTemplate
    extraCtx <- mkExtraCtx id'
    makeItem ""
      >>= loadAndApplyTemplate
            routeTemplate
            (bodyCtx <> extraCtx <> metadataCtx <> defaultContext)
      >>= loadAndApplyTemplate "templates/default.html"
                               (templateCtx <> metadataCtx <> defaultContext)
      >>= relativizeUrls

metadataFieldFrom :: Identifier -> Context a
metadataFieldFrom id' = Context $ \k _ _i -> do
  let empty' =
        noResult
          $  "No '"
          ++ k
          ++ "' field in metadata "
          ++ "of items "
          ++ show id'
  maybe empty' (return . StringField) =<< getMetadataField id' k
