module DepsSensor.Cmd where

import           RIO
import qualified RIO.Text                as T

import qualified Data.ByteArray.Encoding as BA
import           Data.Extensible
import           Data.Fallible
import qualified Data.Yaml               as Y
import           DepsSensor.Env
import qualified GitHub
import qualified Mix.Plugin.GitHub       as MixGitHub
import qualified Mix.Plugin.Logger       as MixLogger

cmd :: RIO Env ()
cmd = do
  repositories <- asks (view #repositories . view #config)
  for_ repositories $ \repo -> evalContT $ do
    let (owner, name) = T.drop 1 <$> T.break (== '/') repo
    content   <- lift (fetchStackFileContent owner name) !?? warn repo "stack.yaml is not found"
    stackFile <- decodeStackFile content ??= warn repo
    resolver  <- toResolver stackFile ??? warn repo "undefined resolver"
    MixLogger.logInfo (display $ repo <> ": " <> resolver)
  where
    warn r msg = exit $ MixLogger.logWarn (display $ T.pack msg <> ": " <> r)

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command.\n"

type StackFile = Record
  '[ "resolver" >: Maybe Text
   , "snapshot" >: Maybe Text
   ]

fetchStackFileContent :: Text -> Text -> RIO Env (Maybe Text)
fetchStackFileContent owner name = do
  let (owner', name') = (GitHub.mkName Proxy owner, GitHub.mkName Proxy name)
  MixLogger.logDebug $ display ("find stack file: " <> owner <> "/" <> name)
  resp <- MixGitHub.fetch $ GitHub.contentsForR owner' name' "stack.yaml" Nothing
  case resp of
    Left _        -> pure Nothing
    Right content -> pure (toFileContent content)

toFileContent :: GitHub.Content -> Maybe Text
toFileContent = \case
  GitHub.ContentFile c | isStackYaml c -> Just $ GitHub.contentFileContent c
  _                                    -> Nothing

decodeStackFile :: Text -> Either String StackFile
decodeStackFile dat = do
  dat' <- BA.convertFromBase BA.Base64 $ T.encodeUtf8 (mconcat $ T.lines dat)
  mapLeft show $ Y.decodeEither' dat'

toResolver :: StackFile -> Maybe Text
toResolver stackFile = stackFile ^. #resolver <|> stackFile ^. #snapshot

isStackYaml :: GitHub.ContentFileData -> Bool
isStackYaml = (== "stack.yaml") . GitHub.contentName . GitHub.contentFileInfo
