module Hakyll.Web.Template.Context.Path
  ( metadataPathFrom
  , getMetadataPath
  , getMetadataPath'
  , metadataContext
  , metadataContext'
  , defaultParseContextField
  )
where

import qualified Data.Aeson                    as Aeson
import           GHC.Exts                       ( toList )
import           Data.Function                  ( fix )
import           Data.Aeson                     ( (.=) )
import           Data.Aeson.Path                ( lookupPath
                                                , readPath'
                                                )
import qualified Data.Yaml                     as Yaml
import qualified Data.ByteString.Char8         as BS8
import qualified Data.Text                     as Text
import           Hakyll.Core.Compiler           ( Compiler
                                                , noResult
                                                )
import           Hakyll.Core.Item               ( Item(..) )
import           Hakyll.Core.Identifier         ( Identifier
                                                , toFilePath
                                                )
import           Hakyll.Core.Metadata           ( getMetadata )
import           Hakyll.Web.Template.Context    ( Context(..)
                                                , ContextField(..)
                                                )

metadataPathFrom
  :: forall a b
   . Aeson.FromJSON b
  => (b -> Compiler ContextField)
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
  maybe empty' pure =<< traverse f =<< getMetadataPath id' k

getMetadataPath
  :: Aeson.FromJSON a => Identifier -> String -> Compiler (Maybe a)
getMetadataPath identifier key = do
  let path = readPath' $ "." <> key
  metadata <- getMetadata identifier
  case lookupPath path (Aeson.Object metadata) of
    Left  e -> error $ show e
    Right v -> case Aeson.fromJSON v of
      Aeson.Error   e  -> error e
      Aeson.Success mv -> pure $ Just mv

getMetadataPath' :: Aeson.FromJSON a => Identifier -> String -> Compiler a
getMetadataPath' identifier key =
  maybe (noResult "not found") pure =<< getMetadataPath identifier key

type ContextFieldParser = Yaml.Value -> Compiler (Maybe ContextField)
metadataContext :: Maybe Aeson.Object -> ContextFieldParser -> Context String
metadataContext mI parseContextField = Context $ \k _ i -> do
  let path                = readPath' $ "." <> k
      missingContextField = noResult $ "Missing field '" ++ k ++ "' in context"
      input               = case mI of
        Just i' -> Aeson.Object i'
        Nothing -> case Yaml.decodeEither' . BS8.pack $ itemBody i of
          Right i' -> i'
          Left  e  -> error $ unlines
            [ toFilePath (itemIdentifier i) <> ": " <> k
            , Yaml.prettyPrintParseException e
            , itemBody i
            ]
  case lookupPath path input of
    Left  _     -> missingContextField
    Right value -> maybe missingContextField pure =<< parseContextField value

defaultParseContextField
  :: Context String
  -> ContextFieldParser
  -> Yaml.Value
  -> Compiler (Maybe ContextField)
defaultParseContextField parentCtx parseContextField = \case
  Aeson.String s -> pure . Just $ StringField (Text.unpack s)
  Aeson.Number n -> pure . Just $ StringField (show n)
  Aeson.Null     -> pure Nothing
  Aeson.Bool b   -> pure $ if b then Just EmptyField else Nothing
  Aeson.Array as ->
    let c      = metadataContext Nothing parseContextField
        mkItem = Item "" . BS8.unpack . Yaml.encode
    in  pure . Just $ ListField (c <> parentCtx) (mkItem <$> toList as)
  Aeson.Object kvs ->
    let c = metadataContext Nothing parseContextField
        mkItem (k', v') = Item "" . BS8.unpack . Yaml.encode $ Aeson.object
          ["key" .= k', "value" .= v']
    in  pure . Just $ ListField (c <> parentCtx) (mkItem <$> toList kvs)

metadataContext' :: Maybe Yaml.Object -> Context String -> Context String
metadataContext' mI ctx =
  metadataContext mI $ fix (defaultParseContextField ctx)
