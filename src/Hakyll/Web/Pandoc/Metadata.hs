module Hakyll.Web.Pandoc.Metadata
  ( readMetadataValue
  , readMetadataValueWith
  , MetadataValue(..)
  )
where

import qualified Data.Text                     as Text
import           Control.Applicative            ( (<|>) )
import           Hakyll.Core.Compiler           ( Compiler )
import           Hakyll.Core.Item               ( Item(..) )
import           Hakyll.Core.Identifier         ( fromFilePath )
import           Data.Aeson                     ( FromJSON(..)
                                                , withText
                                                , withObject
                                                , (.:)
                                                )
import           Hakyll.Web.Pandoc              ( defaultHakyllReaderOptions
                                                , defaultHakyllWriterOptions
                                                , renderPandocWith
                                                )
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

readMetadataValue :: MetadataValue -> Compiler String
readMetadataValue =
  readMetadataValueWith defaultHakyllReaderOptions defaultHakyllWriterOptions

readMetadataValueWith
  :: ReaderOptions -> WriterOptions -> MetadataValue -> Compiler String
readMetadataValueWith readerOptions writerOptions = \case
  RawMetadata s        -> pure s
  PandocMetadata ext s -> itemBody <$> renderPandocWith
    readerOptions
    writerOptions
    Item { itemIdentifier = fromFilePath $ "test" <> ext, itemBody = s }
