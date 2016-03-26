module Message
( user
, encode
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

-- | Basic USER message that gets sent when a connection is opened
user :: String -> String -> Message
user u n = USER u "0" "*" n

-- | Encode a message for transmission
encode :: Message -> B.ByteString
encode msg = convertString $ (++ "\r\n") $ case msg of
  NICK nick -> "NICK " ++ nick
  USER user mode server name -> "USER " ++ intercalate " " [user, mode, server, ":" ++ name] 
  _ -> throw (UnEncodableMessage msg)

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
decode str = either (const $ Unknown (convertString str)) id $
               runParser decodeParser () "Message" str

decodeParser :: Parser Message
decodeParser = parsePing 

parseMsgParams :: Parser [String]
parseMsgParams = do
  raw <- manyTill anyChar eof
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

