module Data.Aeson.Path
  ( JSONPath
  , Value(..)
  , lookupPath
  , pathParser
  , readPath
  , readPath'
  )
where

import           Data.Aeson.Types               ( JSONPath
                                                , Value(..)
                                                , JSONPathElement(..)
                                                )
import qualified Data.HashMap.Strict           as HM
import qualified Data.Vector                   as Vector
import qualified Text.Parsec                   as Parsec
import qualified Text.Parsec.String            as Parsec
import qualified Data.Text                     as Text
import           Control.Applicative

lookupPath :: JSONPath -> Value -> Maybe Value
lookupPath [] v = Just v
lookupPath (Key key : rest) (Object obj) =
  HM.lookup key obj >>= lookupPath rest
lookupPath (Key   _ : _   ) _v         = error "Expected object"
lookupPath (Index i : rest) (Array ls) = ls Vector.!? i >>= lookupPath rest
lookupPath (Index _ : _   ) _v         = error "Expected array"

pathParser :: Parsec.Parser JSONPath
pathParser =
  let keyParser =
          fmap (Key . Text.pack) $ Parsec.char '.' >> Parsec.many Parsec.alphaNum
      indexParser =
          fmap (Index . read)
            . Parsec.between (Parsec.char '[') (Parsec.char ']')
            $ Parsec.many Parsec.digit
  in  Parsec.many (keyParser <|> indexParser) <|> pure []

readPath :: String -> Either String JSONPath
readPath input = case Parsec.parse (pathParser <* Parsec.eof) "" input of
  Left  err -> Left $ show err
  Right a   -> Right a

readPath' :: String -> JSONPath
readPath' input = case readPath input of
  Left  err -> error err
  Right a   -> a
