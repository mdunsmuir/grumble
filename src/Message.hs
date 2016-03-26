module Message
( user
, encode
, decode
, parseIncoming
) where

import Data.String.Conversions
import Data.List (words, intercalate, isPrefixOf)
import qualified Data.ByteString as B
import Data.String.Conversions
import Data.Either
import Text.Parsec
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char
import Grelude

instance Show Message where
  show msg = let shown = convertString $ encode msg
             in  take (length shown - 2) shown

-- | Basic USER message that gets sent when a connection is opened
user :: String -> String -> Message
user u n = Message Nothing USER (Parameters [u, "0", "*", n] Nothing)

-- | Encode a message for transmission
encode :: Message -> B.ByteString
encode Message{..} =
  let Parameters params' mtrail = msgParams
      params = case mtrail of
        Just trail -> params' ++ [':' : trail]
        Nothing -> params'

      cmdStr = case msgCommand of
        Numeric n -> show n
        named -> show named

      parts = case msgPrefix of
        Nothing -> cmdStr : params
        Just pref -> (':' : pref) : cmdStr : params

  in  convertString (intercalate " " parts ++ "\r\n")

-- | Combine some already-received fragments and a fresh fragment into zero
--   or more parsed messages, and any leftover input
parseIncoming :: [B.ByteString] -> B.ByteString -> ([Message], [B.ByteString])
parseIncoming prev new =
  let parseResult = runParser splitParser () "Fragment" new
  in  case parseResult of
        Left err -> ([], prev)
        Right ([], frag) -> ([], frag : prev)
        Right (f1 : ms, f2) -> let first = decode (B.concat (reverse (f1:prev)))
                                   rest = map decode ms
                               in  (first : rest, [f2])
  where
    splitParser :: Parser ([B.ByteString], B.ByteString)
    splitParser = do
      completeMessages <- many (try (manyTill anyChar (try crlf)))
      endingFragment <- manyTill anyChar eof
      return (map convertString completeMessages, convertString endingFragment)

decode :: B.ByteString -> Message
decode str = either (UnDecodable str . show) id $ runParser parseMessage () "message" str

parseMessage :: Parser Message
parseMessage = Message <$> try parsePrefix
                       <*> parseCommand
                       <*> parseParams

parsePrefix :: Parser (Maybe Prefix)
parsePrefix = (char ':' >> (Just <$> manyTill anyChar space)) <|> return Nothing

parseCommand :: Parser Command
parseCommand = try (read <$> many1 letter) <|> (Numeric . read <$> many1 digit)

parseParams :: Parser Parameters
parseParams = do
  many space
  params <- (try trailing <|> many1 (noneOf ['\NUL', '\r', '\n', ' '])) `sepBy` many1 space

  -- I had a LOT of trouble getting parsec to do what I wanted here so rather
  -- than finding a real solution I resorted to this ugly, ugly hack.
  let rparams = reverse params
  return $ case rparams of
    [] -> Parameters [] Nothing
    ps'@(t:ps) -> case t of
                (':':ts) -> Parameters (reverse ps) (Just ts)
                _   -> Parameters (reverse ps') Nothing
  where
    trailing = char ':' >> (':' :) <$> many1 (noneOf ['\NUL', '\r', '\n'])
