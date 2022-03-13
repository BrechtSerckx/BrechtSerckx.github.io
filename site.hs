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

  match "templates/*" $ compile templateBodyCompiler
