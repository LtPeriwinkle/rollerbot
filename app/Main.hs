module Main where

import Control.Monad (when)
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Conversions as C
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import System.Random
import Text.Read
import UnliftIO (liftIO)
import UnliftIO.Concurrent

main :: IO ()
main = runRollerBot

runRollerBot :: IO ()
runRollerBot = do
  tok <- TIO.readFile ".bot-token"
  t <-
    runDiscord $
      def
        { discordToken = tok,
          discordOnEnd = liftIO $ putStrLn "Closing",
          discordOnEvent = eventHandler,
          discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        }
  threadDelay (1 `div` 10 * 10 ^ (6 :: Int))
  TIO.putStrLn t

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (not (isFromBot m) && isRoll m) $ do
    let dieInfo = parseDieInfo $ messageText m
    -- really bad way to get the number out of the Snowflake and turn it into a randomgen seed
    let randList = if isJust dieInfo then genRandList (fromJust dieInfo) (mkStdGen (read $ show (messageId m) :: Int)) else [0]
    let resultsField :: EmbedField
        resultsField = case randList of
          [0] ->
            EmbedField
              { embedFieldName = "Failed to parse dice roll",
                embedFieldValue = "Please format your command as `!roll XdY` where X and Y are positive numbers.",
                embedFieldInline = Just False
              }
          _ ->
            EmbedField
              { embedFieldName = "Results",
                embedFieldValue = C.convertText (concatMap (\x -> if x == "," then ", " else x) (L.group $ init (tail $ show randList))),
                embedFieldInline = Just False
              }
    let embed :: CreateEmbed
        embed =
          def
            { createEmbedTitle = if isJust dieInfo then C.convertText ("rolled " ++ show (fst $ fromJust dieInfo) ++ "d" ++ show (snd $ fromJust dieInfo)) else "Error",
              createEmbedFields = [resultsField]
            }
    let opts :: R.MessageDetailedOpts
        opts =
          def
            { R.messageDetailedAllowedMentions = Just $ def {R.mentionEveryone = False, R.mentionRepliedUser = False},
              R.messageDetailedReference = Just $ def {referenceMessageId = Just $ messageId m},
              R.messageDetailedEmbed = Just embed
            }
    _ <- restCall $ R.CreateMessageDetailed (messageChannel m) opts

    pure ()
  _ -> pure ()

isFromBot :: Message -> Bool
isFromBot m = userIsBot $ messageAuthor m

isRoll :: Message -> Bool
isRoll m = "!roll " `T.isPrefixOf` messageText m

genRandList :: (Int, Int) -> StdGen -> [Int]
genRandList (a, b)
  | a == 0 || b == 0 = empty
  | otherwise = take a . L.unfoldr (Just . uniformR (1, b))

-- lol
empty :: StdGen -> [Int]
empty _ = [0]

parseDieInfo :: T.Text -> Maybe (Int, Int)
parseDieInfo m = do
  let strs = span (/= 'd') $ dropWhile (`elem` ("!roll " :: [Char])) (C.convertText m :: String)
  if snd strs /= ""
    then
      let count = readMaybe $ fst strs :: Maybe Int
          sides = readMaybe (tail $ snd strs) :: Maybe Int
       in if isJust count && isJust sides then Just (fromJust count, fromJust sides) else Nothing
    else Nothing
