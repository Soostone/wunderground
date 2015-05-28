{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module WUnderground.Client
    ( -- * Weather conditions API
      withWU
    , coordinateConditions
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception     as E
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Default.Class
import           Data.Monoid
import           Data.Text.Strict.Lens (packed, utf8)
import           Network.HTTP.Client
import           Network.HTTP.Types    (Method)
import           URI.ByteString
-------------------------------------------------------------------------------
import           WUnderground.Types
-------------------------------------------------------------------------------


-- | Convenience function for when you don't have an externally
-- configured manager and want to create one on the spot.
withWU :: ManagerSettings -> APIKey -> WU IO a -> IO a
withWU ms k f = withManager ms $ \mgr -> do
  let conf = defaultWUConfig mgr k
  runWU conf f


-------------------------------------------------------------------------------
coordinateConditions
    :: (MonadWU m)
    => Lat
    -> Lng
    -> m ObservationResponse
coordinateConditions lt lg = makeRequest "GET" path
  where
    path = "/geolookup/conditions/q/" <> showBS (lt ^. lat) <> "," <> showBS (lg ^. lng) <> ".json"


-------------------------------------------------------------------------------
makeRequest
    :: ( FromJSON a
       , MonadWU m)
    => Method
    -> ByteString
    -> m a
makeRequest meth p = do
    cfg <- getWUConfig
    --TODO: consolidate?
    let k = cfg ^. wuAPIKey . apiKeyText . re utf8
    let pathAppend = k <> p
    let bu = cfg ^. wuBaseURI
    let finalURI = bu & uriPathL <>~ ("/" <> pathAppend)
    req <- setURI meth def finalURI
    httpRequest <- _wuHttpRequest <$> getWUConfig
    res <- liftIO $ E.try $ httpRequest req (cfg ^. wuManager)
    body <- either handleHttpException return res
    let parsed = eitherDecode body
    either (throwM . WUParseError . view packed) return parsed
  where
    handleHttpException = throwM . WUHttpException


-------------------------------------------------------------------------------
setURI :: MonadThrow m => Method -> Request -> URI -> m Request
setURI meth req URI{..} = do
  Authority {..} <- maybe missingUA return uriAuthority
  let req' = req { secure = isSecure
                 , host   = hostBS authorityHost
                 , port   = thePort
                 , path   = uriPath
                 , method = meth
                 }
      thePort = maybe defPort portNumber authorityPort
      addAuth = maybe id addAuth' authorityUserInfo
  return $ setQueryString theQueryString $ addAuth req'
  where
    missingUA = throwM $ InvalidUrlException "N/A" "Missing URI host/port"
    addAuth' UserInfo {..} = applyBasicProxyAuth uiUsername uiPassword
    defPort
      | isSecure  = 443
      | otherwise = 80
    isSecure = case uriScheme of
      Scheme "https" -> True
      _              -> False
    theQueryString = [(k , Just v) | (k, v) <- queryPairs uriQuery]


-------------------------------------------------------------------------------
showBS :: Show a => a -> ByteString
showBS = BS.pack . show
