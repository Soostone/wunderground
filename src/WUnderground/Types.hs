{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
module WUnderground.Types
    (-- * Client Types
      WUConfig(..)
    , wuManager
    , wuBaseURI
    , wuAPIKey
    , defaultWUConfig
    , APIKey(..)
    , apiKeyText
    , MonadWU(..)
    , WU(..)
    , runWU
    , WUError(..)
    -- * API Types
    , Observation
    , obsImage
    , obsDisplayLoc
    , obsTime
    , obsLocalTime
    , obsWeather
    , obsTempF
    , obsTempC
    , obsWindMPH
    , obsWindGustMPH
    , obsWindKPH
    , obsWindGustKPH
    , obsDewpointF
    , obsDewpointC
    , obsFeelsLikeF
    , obsFeelsLikeC
    , Image(..)
    , imageUrl
    , imageTitle
    , imageLink
    , DisplayLocation(..)
    , locationFull
    , loationCity
    , loationState
    , loationStateName
    , loationCountry
    , loationCountryISO
    , loationZip
    , loationLat
    , loationLng
    , Lat(..)
    , lat
    , Lng(..)
    , lng
    , TempF(..)
    , tempF
    , TempC(..)
    , tempC
    , MPH(..)
    , mph
    , KPH(..)
    , kph
    , Weather(..)
    , weatherText
    , LocationFullName(..)
    , locationFullNameText
    , City(..)
    , cityText
    , GeoState(..)
    , geoStateText
    , GeoStateName(..)
    , geoStateNameText
    , Country(..)
    , countryText
    , CountryISO3166(..)
    , countryISO3166Text
    , PostalCode(..)
    , postalCodeText
    -- * Responses
    , ObservationResponse(..)
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Aeson
import           Data.Aeson.Types      (typeMismatch)
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific
import           Data.Text             (Text)
import           Data.Text.Strict.Lens (packed)
import           Data.Time
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Typeable
import           GHC.Generics          (Generic)
import           Network.HTTP.Client
import           URI.ByteString
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Client Types
-------------------------------------------------------------------------------
data WUConfig = WUConfig {
      _wuManager :: Manager
    -- ^ Note that if you provide an alternative URI that uses SSL, you should use manager settings from @http-client-tls@
    , _wuBaseURI :: URI
    , _wuAPIKey  :: APIKey
    } deriving (Typeable)


-------------------------------------------------------------------------------
-- | Uses the default wunderground base URI
-- (http://api.wunderground.com/api)
defaultWUConfig :: Manager -> APIKey -> WUConfig
defaultWUConfig mgr = WUConfig mgr defaultURI
  where
    defaultURI = URI (Scheme "http")
                     (Just $ Authority Nothing (Host "api.wunderground.com") Nothing)
                     "/api"
                     mempty
                     Nothing



-------------------------------------------------------------------------------
newtype APIKey = APIKey {
      _apiKeyText :: Text
    } deriving (Show, Eq, Typeable, Generic)


-------------------------------------------------------------------------------
-- | All API calls operate within MonadWU. The idea is that it can be
-- easily embedded in your own monad transformer stack. A default
-- instance for a ReaderT and alias 'WU' is provided for the simple
-- case.
class ( Functor m
      , Applicative m
      , MonadThrow m
      , MonadIO m) => MonadWU m where
  getWUConfig :: m WUConfig


newtype WU m a = WU {
      unWU :: ReaderT WUConfig m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadState s
               , MonadWriter w
               , MonadPlus
               , MonadFix)

instance MonadTrans WU where
  lift = WU . lift

instance (MonadReader r m) => MonadReader r (WU m) where
    ask = lift ask
    local f (WU (ReaderT m)) = WU $ ReaderT $ \r ->
      local f (m r)

instance (Functor m, Applicative m, MonadIO m, MonadThrow m) => MonadWU (WU m) where
  getWUConfig = WU getWUConfig


instance (Functor m, Applicative m, MonadIO m, MonadThrow m) => MonadWU (ReaderT WUConfig m) where
  getWUConfig = ask


-------------------------------------------------------------------------------
runWU :: WUConfig -> WU m a -> m a
runWU e f = runReaderT (unWU f) e


-------------------------------------------------------------------------------
data WUError = WUHttpException HttpException
             | WUParseError Text
               deriving (Show, Typeable, Generic)


instance Exception WUError


-------------------------------------------------------------------------------
-- API Types
-------------------------------------------------------------------------------
data Observation = Observation {
      _obsImage       :: Image
    , _obsDisplayLoc  :: DisplayLocation
    , _obsTime        :: UTCTime
    , _obsLocalTime   :: UTCTime
    , _obsWeather     :: Weather
    , _obsTempF       :: TempF
    , _obsTempC       :: TempC
    , _obsWindMPH     :: MPH
    , _obsWindGustMPH :: MPH
    , _obsWindKPH     :: KPH
    , _obsWindGustKPH :: KPH
    , _obsDewpointF   :: TempF
    , _obsDewpointC   :: TempC
    , _obsFeelsLikeF  :: TempF
    , _obsFeelsLikeC  :: TempC
    } deriving (Show, Eq, Generic, Typeable)


instance FromJSON Observation where
  parseJSON = withObject "Observation" parse
    where
      parse o = Observation
        <$> o .: "image"
        <*> o .: "display_location"
        <*> (unEpoch <$> o .: "observation_epoch")
        <*> (unEpoch <$> o .: "local_epoch")
        <*> o .: "weather"
        <*> (TempF . unDJS <$> o .: "temp_f")
        <*> (TempC . unDJS <$> o .: "temp_c")
        <*> (MPH . unDJS <$> o .: "wind_mph")
        <*> (MPH . unDJS <$> o .: "wind_gust_mph")
        <*> (KPH . unDJS <$> o .: "wind_kph")
        <*> (KPH . unDJS <$> o .: "wind_gust_kph")
        <*> (TempF . unDJS <$> o .: "dewpoint_f")
        <*> (TempC . unDJS <$> o .: "dewpoint_c")
        <*> (TempF . unDJS <$> o .: "feelslike_f")
        <*> (TempC . unDJS <$> o .: "feelslike_c")


-------------------------------------------------------------------------------
data Image = Image {
      _imageUrl   :: Text
    , _imageTitle :: Text
    , _imageLink  :: Text
    } deriving (Show, Eq, Generic, Typeable)


instance FromJSON Image where
  parseJSON = withObject "Image" parse
    where
      parse o = Image
        <$> o .: "url"
        <*> o .: "title"
        <*> o .: "link"



-------------------------------------------------------------------------------
data DisplayLocation = DisplayLocation {
      _locationFull      :: LocationFullName
    , _loationCity       :: City
    , _loationState      :: GeoState
    , _loationStateName  :: GeoStateName
    , _loationCountry    :: Country
    , _loationCountryISO :: CountryISO3166
    , _loationZip        :: PostalCode
    --TODO: magic? wmo?
    , _loationLat        :: Lat
    , _loationLng        :: Lng
    } deriving (Show, Eq, Generic, Typeable)


instance FromJSON DisplayLocation where
  parseJSON = withObject "DisplayLocation" parse
    where
      parse o = DisplayLocation
        <$> o .: "full"
        <*> o .: "city"
        <*> o .: "state"
        <*> o .: "state_name"
        <*> o .: "country"
        <*> o .: "country_iso3166"
        <*> o .: "zip"
        <*> o .: "latitude"
        <*> o .: "longitude"


-------------------------------------------------------------------------------
newtype LocationFullName = LocationFullName {
      _locationFullNameText :: Text
    } deriving (Show, Eq, Ord, Generic, Typeable, FromJSON)


-------------------------------------------------------------------------------
newtype City = City {
      _cityText :: Text
    } deriving (Show, Eq, Ord, Generic, Typeable, FromJSON)


-------------------------------------------------------------------------------
-- | GeoState as opposed to the State monad
newtype GeoState = GeoState {
      _geoStateText :: Text
    } deriving (Show, Eq, Ord, Generic, Typeable, FromJSON)


-------------------------------------------------------------------------------
newtype GeoStateName = GeoStateName {
      _geoStateNameText :: Text
    } deriving (Show, Eq, Ord, Generic, Typeable, FromJSON)


-------------------------------------------------------------------------------
newtype Country = Country {
      _countryText :: Text
    } deriving (Show, Eq, Ord, Generic, Typeable, FromJSON)


-------------------------------------------------------------------------------
--TODO: smart constructor
newtype CountryISO3166 = CountryISO3166 {
      _countryISO3166Text :: Text
    } deriving (Show, Eq, Ord, Generic, Typeable, FromJSON)


-------------------------------------------------------------------------------
newtype PostalCode = PostalCode {
      _postalCodeText :: Text
    } deriving (Show, Eq, Ord, Generic, Typeable, FromJSON)


-------------------------------------------------------------------------------
newtype Lat = Lat {
      _lat :: Double
    } deriving (Show, Eq, Ord, Generic, Typeable)


instance FromJSON Lat where
  parseJSON v = Lat . unDJS <$> parseJSON v


-------------------------------------------------------------------------------
newtype Lng = Lng {
      _lng :: Double
    } deriving (Show, Eq, Ord, Generic, Typeable)


instance FromJSON Lng where
  parseJSON v = Lng . unDJS <$> parseJSON v


-------------------------------------------------------------------------------
newtype TempF = TempF {
      _tempF :: Double
    } deriving (Show, Eq, Ord, Generic, Typeable)


-------------------------------------------------------------------------------
newtype TempC = TempC {
      _tempC :: Double
    } deriving (Show, Eq, Ord, Generic, Typeable)


-------------------------------------------------------------------------------
newtype MPH = MPH {
      _mph :: Double
    } deriving (Show, Eq, Ord, Generic, Typeable)


-------------------------------------------------------------------------------
newtype KPH = KPH {
      _kph :: Double
    } deriving (Show, Eq, Ord, Generic, Typeable)


--------------------pp-----------------------------------------------------------
newtype Weather = Weather {
      _weatherText :: Text
    } deriving (Show, Eq, Ord, Generic, Typeable, FromJSON)


-------------------------------------------------------------------------------
-- Response Types
-------------------------------------------------------------------------------
data ObservationResponse = ObservationResponse {
      responseObservation :: Maybe Observation
    } deriving (Show, Eq, Generic, Typeable)


instance FromJSON ObservationResponse where
  parseJSON = withObject "ObservationResponse" parse
    where
      parse o = do
        resp <- o .: "response"
        me <- optional $ resp .: "error"
        case me of
          Just e
            | responseErrorType e == "querynotfound" -> return $ ObservationResponse Nothing
            | otherwise -> fail $ "Unexpected error: " ++ show e
          Nothing -> ObservationResponse . Just <$> o .: "current_observation"


-------------------------------------------------------------------------------
data ResponseError = ResponseError {
      responseErrorType        :: Text
    , responseErrorDescription :: Text
    } deriving (Show, Eq, Ord, Generic, Typeable)


instance FromJSON ResponseError where
  parseJSON = withObject "ResponseError" parse
    where
      parse o = ResponseError
        <$> o .: "type"
        <*> o .: "description"


-------------------------------------------------------------------------------
-- | Newtype wrapper to parse string unix epochs
newtype Epoch = Epoch {
      unEpoch :: UTCTime
    }


instance FromJSON Epoch where
  parseJSON v = case v of
      (String t) -> maybe (fail "Invalid epoch") (return . go) (t ^. from packed ^? _Show)
      (Number n) -> maybe (fail "epoch out of range") (return . go) (parseInt n)
      x -> typeMismatch "String | Number" x
    where
      parseInt :: Scientific -> Maybe Int
      parseInt = toBoundedInteger
      go n = Epoch $ posixSecondsToUTCTime $ fromIntegral n


-------------------------------------------------------------------------------
-- | Newtype wrapper to parse sometimes-numbers-sometimes-strings shennanigans
newtype DoubleJS = DoubleJS {
      unDJS :: Double
    }


instance FromJSON DoubleJS where
  parseJSON v = case v of
      (String t) -> maybe (fail "Invalid double") (return . DoubleJS) (t ^. from packed ^? _Show)
      (Number n) -> maybe (fail "double out of range") (return . DoubleJS) (parseDouble n)
      x -> typeMismatch "String | Number" x
    where
      parseDouble :: Scientific -> Maybe Double
      parseDouble = hush . toBoundedRealFloat


-------------------------------------------------------------------------------
hush :: Either l r -> Maybe r
hush (Right x) = Just x
hush _         = Nothing


-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------
makeLenses ''WUConfig
makeLenses ''APIKey
makeLenses ''Observation
makeLenses ''Image
makeLenses ''DisplayLocation
makeLenses ''LocationFullName
makeLenses ''City
makeLenses ''GeoState
makeLenses ''GeoStateName
makeLenses ''Country
makeLenses ''CountryISO3166
makeLenses ''PostalCode
makeLenses ''Lat
makeLenses ''Lng
makeLenses ''TempF
makeLenses ''TempC
makeLenses ''MPH
makeLenses ''KPH
makeLenses ''Weather
