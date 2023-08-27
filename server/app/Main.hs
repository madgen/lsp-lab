{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import           Language.LSP.Server
import           Language.LSP.Protocol.Types
import           Language.LSP.Protocol.Message
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Maybe (fromJust)
import           Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Char (isAlpha)
import           GHC.IO.Exception (ExitCode(ExitFailure))
import           System.Exit (exitWith)

main :: IO ()
main = do
  exitCode <- runServer serverDef
  exitWith $ ExitFailure exitCode

serverDef :: ServerDefinition ()
serverDef =
  ServerDefinition { onConfigurationChange = const $ const $ Right ()
                   , defaultConfig = ()
                   , doInitialize = \env _req -> pure $ Right env
                   , staticHandlers = handlers
                   , interpretHandler = \env -> Iso (runLspT env) liftIO
                   , options = defaultOptions
                   }
  where
    handlers :: Handlers (LspM ())
    handlers = mconcat
      [ notificationHandler SMethod_Initialized
        $ \_ -> do
          sendNotification SMethod_WindowLogMessage
            $ LogMessageParams MessageType_Log "initialized"
      , requestHandler SMethod_TextDocumentHover
        $ \req responder -> do
          let TRequestMessage
                _
                _
                _
                (HoverParams (TextDocumentIdentifier uri) pos _workDone) = req
          let path = fromJust $ uriToFilePath uri
          contents <- liftIO $ TIO.readFile path
          let Position line col = pos
          case findWordAndBoundaries
            contents
            (fromIntegral line)
            (fromIntegral col) of
            Just (word, leftCol, rightCol) -> do
              let ms = mkMarkdown
                    $ "length('"
                    <> word
                    <> "') = "
                    <> fromString (show (T.length word))
              let range = Range
                    (Position line $ fromIntegral leftCol)
                    (Position line $ fromIntegral rightCol)
              let rsp = Hover (InL ms) (Just range)
              responder (Right $ InL rsp)
            Nothing -> responder (Right $ InR Null)]

findWordAndBoundaries :: T.Text -> Int -> Int -> Maybe (T.Text, Int, Int)
findWordAndBoundaries contents line col
  | isAlpha c = Just
    ( leftFragment <> rightFragment
    , col - T.length leftFragment
    , col + T.length rightFragment)
  | otherwise = Nothing
  where
    c = T.index r 0

    (l, r) = T.splitAt col lineContents

    leftFragment = T.reverse . T.takeWhile isAlpha . T.reverse $ l

    rightFragment = T.takeWhile isAlpha r

    lineContents = T.lines contents !! line