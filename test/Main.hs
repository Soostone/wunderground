{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Lens
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Lazy  as LB
import           Data.Time.Clock.POSIX
import           Network.HTTP.Client
import           Test.Tasty
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           WUnderground
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain testSuite


-------------------------------------------------------------------------------
testSuite :: TestTree
testSuite = testGroup "wunderground"
  [
    clientTests
  ]


-------------------------------------------------------------------------------
clientTests :: TestTree
clientTests = testGroup "client"
  [
    coordinateConditionsTests
  ]


-------------------------------------------------------------------------------
coordinateConditionsTests :: TestTree
coordinateConditionsTests = testGroup "coordinateConditions"
  [
    testCase "roundtrips correctly" $ do
       let lt = Lat 40.7577
       let lg = Lng (-73.9857)
       let expectedPath = "/api/xxxx/geolookup/conditions/q/40.7577,-73.9857.json"
       resp <- stubRequestWithFixture "GET" expectedPath "nyc.json" $ coordinateConditions lt lg
       resp @?= nycResponse
  ]


-------------------------------------------------------------------------------
nycResponse :: ObservationResponse
nycResponse = ObservationResponse (Just obs)
  where
    obs = Observation img loc time ltime weather tF tC wMPH wgMPH wKPH wgKPH dpF dpC flF flC
    img = Image "http://icons.wxug.com/graphics/wu2/logo_130x80.png" "Weather Underground" "http://www.wunderground.com"
    loc = DisplayLocation lfn city st stn c ciso z lt lg
    lfn = LocationFullName "New York, NY"
    city = City "New York"
    st = GeoState "NY"
    stn = GeoStateName "New York"
    c = Country "US"
    ciso = CountryISO3166 "US"
    z = PostalCode "10172"
    lt = Lat 40.75
    lg = Lng (-73.98)
    time = posixSecondsToUTCTime 1432851597
    ltime = posixSecondsToUTCTime 1432851619
    weather = Weather "Scattered Clouds"
    tF = TempF 82.9
    tC = TempC 28.3
    wMPH = MPH 1
    wgMPH = MPH 4
    wKPH = KPH 1.6
    wgKPH = KPH 6.4
    dpF = TempF 68
    dpC = TempC 20
    flF = TempF 86
    flC = TempC 30


-------------------------------------------------------------------------------
apiKey :: APIKey
apiKey = APIKey "xxxx"

-------------------------------------------------------------------------------
stubRequestWithFixture :: ByteString -> ByteString -> FilePath -> WU IO a -> IO a
stubRequestWithFixture expectedMeth expectedPath fixtureFile f = do
    body <- LB.readFile fixturePath
    withManager defaultManagerSettings $ \mgr -> do
      let conf = defaultWUConfig mgr apiKey & wuHttpRequest .~ responder body
      runWU conf f
  where
    responder body req _mgr = do
      method req @?= expectedMeth
      secure req @?= False
      host req @?= "api.wunderground.com"
      path req @?= expectedPath
      return body
    fixturePath = "test/fixtures/" ++ fixtureFile
