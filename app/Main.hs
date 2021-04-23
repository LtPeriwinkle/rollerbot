{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import System.Random
import qualified Data.Text as T
import qualified Data.Text.Conversions as C
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import Discord
import qualified Discord.Requests as R
import Discord.Types
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
    let randList = genRandList dieInfo (mkStdGen (read $ show (messageId m) :: Int))
    let opts :: R.MessageDetailedOpts
        opts =
          def
            { R.messageDetailedContent = C.convertText ("rolling " ++ show (fst dieInfo) ++ "d" ++ show (snd dieInfo) ++ ", results: " ++ show randList) :: T.Text,
              R.messageDetailedAllowedMentions = Just $ def {R.mentionEveryone = False, R.mentionRepliedUser = False},
              R.messageDetailedReference = Just $ def {referenceMessageId = Just $ messageId m}
            }
    _ <- restCall $ R.CreateMessageDetailed (messageChannel m) opts

    pure ()
  _ -> pure ()

isFromBot :: Message -> Bool
isFromBot m = userIsBot $ messageAuthor m

isRoll :: Message -> Bool
isRoll m = "!roll" `T.isPrefixOf` messageText m

genRandList :: (Int, Int) -> StdGen -> [Int]
genRandList (a, b)
    | a == 0 || b == 0 = empty
    | otherwise = take a . L.unfoldr (Just . uniformR (0, b))
-- lol
empty :: StdGen -> [Int]
empty g = []

parseDieInfo :: T.Text -> (Int, Int)
parseDieInfo m = do
    let split = span (/='d') $ dropWhile (`elem` ("!roll " :: [Char])) (C.convertText m :: String)
    (read (fst split) :: Int, read (tail $ snd split) :: Int)

