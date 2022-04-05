module Hakyll.Web.Template.Context.Path
  ( metadataPathFrom
  , getMetadataPath
  , getMetadataPath'
  )
where

import           Hakyll.Core.Identifier         ( Identifier )
import           Hakyll.Web.Template.Context    ( Context(..)
                                                , ContextField(..)
                                                )
import           Hakyll.Core.Compiler           ( Compiler
                                                , noResult
                                                )
import           Hakyll.Core.Metadata           ( getMetadata )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Path                ( lookupPath
                                                , readPath'
                                                )

metadataPathFrom
  :: forall a b
   . Aeson.FromJSON b
  => (b -> Compiler String)
  -> Identifier
  -> Context a
metadataPathFrom f id' = Context $ \k _ _i -> do
  let empty' =
        noResult
          $  "No '"
          ++ k
          ++ "' field in metadata "
          ++ "of items "
          ++ show id'
  maybe empty' (return . StringField) =<< traverse f =<< getMetadataPath id' k

getMetadataPath
  :: Aeson.FromJSON a => Identifier -> String -> Compiler (Maybe a)
getMetadataPath identifier key = do
  let path = readPath' $ "." <> key
  metadata <- getMetadata identifier
  pure $ do
    v <- lookupPath path $ Aeson.Object metadata
    case Aeson.fromJSON v of
      Aeson.Error   e  -> error e
      Aeson.Success mv -> Just mv

getMetadataPath' :: Aeson.FromJSON a => Identifier -> String -> Compiler a
getMetadataPath' identifier key =
  maybe (noResult "not found") pure =<< getMetadataPath identifier key
