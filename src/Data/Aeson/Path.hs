module Data.Aeson.Path
  ( JSONPath
  , Value(..)
  , lookupPath
  , pathParser
  , readPath
  , readPath'
  )
where

import           Control.Applicative            ( (<|>) )
import           Data.Aeson.Types               ( JSONPath
                                                , JSONPathElement(..)
                                                , Value(..)
                                                , Object
                                                )
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as Text
import qualified Data.Vector                   as Vector
import           Data.Vector                    ( Vector )
import qualified Text.Parsec                   as Parsec
import qualified Text.Parsec.String            as Parsec
import           Data.Text                      ( Text )

data PathLookupError
  = IndexError Int (Vector Value)
  | KeyError Text Object
  | ExpectedArray Int Value
  | ExpectedObject Text Value
  deriving Show

lookupPath :: JSONPath -> Value -> Either PathLookupError Value
lookupPath []               v            = Right v
lookupPath (Key key : rest) (Object obj) = case HM.lookup key obj of
  Nothing -> Left $ KeyError key obj
  Just v  -> lookupPath rest v
lookupPath (Key   key : _   ) v          = Left $ ExpectedObject key v
lookupPath (Index i   : rest) (Array ls) = case ls Vector.!? i of
  Nothing -> Left $ IndexError i ls
  Just v  -> lookupPath rest v
lookupPath (Index i : _) v = Left $ ExpectedArray i v

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
