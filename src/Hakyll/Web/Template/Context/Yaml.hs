module Hakyll.Web.Template.Context.Yaml
  ( getYamlMetadataPath
  , getYamlMetadataPath'
  , yamlMetadataPathFrom
  )
where

import           Hakyll.Core.Identifier         ( Identifier
                                                , toFilePath
                                                )
import           Hakyll.Web.Template.Context    ( Context(..)
                                                , ContextField(..)
                                                )
import           Hakyll.Core.Compiler           ( Compiler
                                                , unsafeCompiler
                                                , noResult
                                                )
import           Hakyll.Core.Metadata           ( Metadata )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Path                ( lookupPath
                                                , readPath'
                                                )
import qualified Data.Yaml                     as Yaml

getYamlMetadata :: Identifier -> Compiler Metadata
getYamlMetadata id' = unsafeCompiler $ do
  let fp = toFilePath id'
  Yaml.decodeFileThrow fp

getYamlMetadataPath
  :: Aeson.FromJSON a => Identifier -> String -> Compiler (Maybe a)
getYamlMetadataPath identifier key = do
  let path = readPath' $ "." <> key
  metadata <- getYamlMetadata identifier
  pure $ do
    v <- lookupPath path $ Aeson.Object metadata
    case Aeson.fromJSON v of
      Aeson.Error   e  -> error e
      Aeson.Success mv -> Just mv

getYamlMetadataPath' :: Aeson.FromJSON a => Identifier -> String -> Compiler a
getYamlMetadataPath' identifier key =
  maybe (noResult "not found") pure =<< getYamlMetadataPath identifier key

yamlMetadataPathFrom
  :: forall a b
   . Aeson.FromJSON b
  => (b -> Compiler String)
  -> Identifier
  -> Context a
yamlMetadataPathFrom f id' = Context $ \k _ _i -> do
  let empty' =
        noResult
          $  "No '"
          ++ k
          ++ "' field in metadata "
          ++ "of item "
          ++ show id'
  maybe empty' (return . StringField)
    =<< traverse f
    =<< getYamlMetadataPath id' k
