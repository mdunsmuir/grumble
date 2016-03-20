module Message
( user
, encode
, decode
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

user :: String -> String -> Message
user u n = USER u "0" "*" n

encode :: Message -> B.ByteString
encode msg = convertString $ (++ "\r\n") $ case msg of
  NICK nick -> "NICK " ++ nick
  USER user mode server name -> "USER " ++ intercalate " " [user, mode, server, ":" ++ name] 
  _ -> throw (UnEncodableMessage msg)

splitOnCRLF :: B.ByteString -> [B.ByteString]
splitOnCRLF bstr
  | B.null bstr = []
  -- | bstr == "\r\n" = []
  | otherwise = let (pre, match) = B.breakSubstring "\r\n" bstr
                in  pre : splitOnCRLF (B.drop 2 match)

decode :: B.ByteString -> [Message]
decode str = let raws = splitOnCRLF str
                 f raw = either (const (Unknown (convertString raw))) id $
                           runParser decodeParser () "Message" raw
             in  map f raws

decodeParser :: Parser Message
decodeParser = parsePing 

parseMsgParams :: Parser [String]
parseMsgParams = do
  raw <- manyTill anyChar (try crlf)
  let parts = words raw
      f :: [String] -> [String]
      f [] = []
      f (s:ss)
        | ":" `isPrefixOf` s = [intercalate " " (tail s : ss)]
        | otherwise = s : f ss
  return (f parts)

parsePing :: Parser Message
parsePing = do
  string "PING"
  params <- parseMsgParams
  if length params /= 1
    then parserFail "Too many or too few parameters in PING message!"
    else let [msg] = params in return (PING msg)
