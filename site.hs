module Main where

import           Hakyll

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

  create ["index.html"] $ do
    route idRoute
    compile $ do
      let bodyCtx     = field "body" $ \_ -> loadBody "data/index.md"
          metadataCtx = metadataFieldFrom "data/index.md"
          templateCtx = metadataFieldFrom "templates/index.html"
      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html"
                                 (bodyCtx <> metadataCtx <> defaultContext)
        >>= loadAndApplyTemplate
              "templatesDefault.html"
              (templateCtx <> metadataCtx <> defaultContext)
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

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
