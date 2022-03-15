module Main where

import           Hakyll
import           Data.List                      ( sortOn )

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

  create ["about.html"] $ do
    route idRoute
    compile $ do
      let bodyCtx     = field "body" $ \_ -> loadBody "data/about.md"
          metadataCtx = metadataFieldFrom "data/about.md"
          templateCtx = metadataFieldFrom "templates/about.html"
      makeItem ""
        >>= loadAndApplyTemplate "templates/about.html"
                                 (bodyCtx <> metadataCtx <> defaultContext)
        >>= loadAndApplyTemplate
              "templatesDefault.html"
              (templateCtx <> metadataCtx <> defaultContext)
        >>= relativizeUrls

  create ["professional.html"] $ do
    route idRoute
    compile $ do
      items <- sortOn itemIdentifier <$> loadAll "data/professional/*"
      let bodyCtx     = field "body" $ \_ -> loadBody "data/professional.md"
          itemsCtx    = listField "items" defaultContext (pure items)
          metadataCtx = metadataFieldFrom "data/professional.md"
          templateCtx = metadataFieldFrom "templates/professional.html"
      makeItem ""
        >>= loadAndApplyTemplate
              "templates/professional.html"
              (bodyCtx <> itemsCtx <> metadataCtx <> defaultContext)
        >>= loadAndApplyTemplate
              "templatesDefault.html"
              (templateCtx <> metadataCtx <> defaultContext)
        >>= relativizeUrls

  create ["projects.html"] $ do
    route idRoute
    compile $ do
      let projectsId       = "data/projects.md"
          projectsTemplate = "templates/projects.html"
          defaultTemplate  = "templates/default.html"
      items <- sortOn itemIdentifier <$> loadAll "data/projects/*"
      makeItem ""
        >>= loadAndApplyTemplate
              projectsTemplate
              (  bodyFieldFrom projectsId
              <> listField "items" defaultContext (pure items)
              <> metadataFieldFrom [projectsId]
              <> defaultContext
              )
        >>= loadAndApplyTemplate
              defaultTemplate
              (  metadataFieldFrom [projectsTemplate, projectsId]
              <> defaultContext
              )
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
