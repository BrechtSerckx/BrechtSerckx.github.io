import           Hakyll

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- loadAll "posts/*"
      let indexCtx =
            listField "posts" defaultContext (return posts)
              `mappend` defaultContext

      getResourceBody >>= applyAsTemplate indexCtx >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

-- postCtx = pathField "id" `mappend` defaultContext
postCtx = defaultContext
