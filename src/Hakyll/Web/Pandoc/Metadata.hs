module Hakyll.Web.Pandoc.Metadata
  ( readMetadataValue
  , readMetadataValueWith
  , MetadataValue(..)
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(..)
                                                , withObject
                                                , withText
                                                )
import qualified Data.Text                     as Text
import           Hakyll.Core.Compiler           ( Compiler )
import           Hakyll.Core.Identifier         ( fromFilePath )
import           Hakyll.Core.Item               ( Item(..) )
import           Hakyll.Web.Pandoc              ( defaultHakyllReaderOptions
                                                , defaultHakyllWriterOptions
                                                , renderPandocWith
                                                )
import           Hakyll.Web.Template.Context    ( ContextField(..) )
import           Text.Pandoc.Options            ( ReaderOptions
                                                , WriterOptions
                                                )

data MetadataValue = RawMetadata String | PandocMetadata FilePath String

instance FromJSON MetadataValue where
  parseJSON v =
    let parseRaw    = withText "RawMetadata" $ pure . RawMetadata . Text.unpack
        parsePandoc = withObject "PandocMetadata"
          $ \o -> PandocMetadata <$> o .: "pandoc" <*> o .: "value"
    in  parseRaw v <|> parsePandoc v

readMetadataValue :: MetadataValue -> Compiler ContextField
readMetadataValue =
  readMetadataValueWith defaultHakyllReaderOptions defaultHakyllWriterOptions

readMetadataValueWith
  :: ReaderOptions -> WriterOptions -> MetadataValue -> Compiler ContextField
readMetadataValueWith readerOptions writerOptions = \case
  RawMetadata s        -> pure $ StringField s
  PandocMetadata ext s -> StringField . itemBody <$> renderPandocWith
    readerOptions
    writerOptions
    Item { itemIdentifier = fromFilePath $ "test" <> ext, itemBody = s }
