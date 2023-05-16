{-# LANGUAGE OverloadedStrings #-}

module ProductsHandlerSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Test.Hspec.Wai
import Database.MySQL.Simple
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Control.Monad.IO.Class
import Network.Wai.Test
import Network.HTTP.Types
import App
import Control.Monad
import Data.Maybe
import qualified Data.Aeson as Aeson
import qualified Security.Jwt as Jwt

shouldRespondWithPredicate :: Control.Monad.IO.Class.MonadIO m => m SResponse -> (TL.Text -> Bool, String) -> m ()
shouldRespondWithPredicate action (matcher, errorMessage) = do
  r <- action
  if matcher (EL.decodeUtf8  (simpleBody r))
    then mapM_ (liftIO . expectationFailure) []
    else mapM_ (liftIO . expectationFailure) [errorMessage]

cleanDb :: Connection -> IO ()
cleanDb dbConn =
  execute dbConn "TRUNCATE TABLE users" () >> 
  execute dbConn "TRUNCATE TABLE permissions" () >>
  execute dbConn "TRUNCATE TABLE users_permissions" () >>
  execute dbConn "INSERT INTO permissions (permission) VALUES ('userLevel');" () >>
  execute dbConn "INSERT INTO permissions (permission) VALUES ('adminLevel')" () >>
  execute dbConn "TRUNCATE TABLE products" () >>
  execute dbConn "TRUNCATE TABLE images" () >>
  execute dbConn "TRUNCATE TABLE products_images" () >>
  execute dbConn "TRUNCATE TABLE dimensions" () >>
  execute dbConn "TRUNCATE TABLE dimensions_images" () >>
  return ()

createProduct :: Connection -> IO ()
createProduct dbConn = 
  execute dbConn "INSERT INTO products (name, description, price_formula) VALUES ('table', 'just a table', '{height} * {width} * {depth}')" () >>
  execute dbConn "INSERT INTO images (url) VALUES ('https://tiozao.co/image1.png')" () >>
  execute dbConn "INSERT INTO products_images (product_id, image_id) VALUES (1, 1)" () >>
  execute dbConn "INSERT INTO dimensions (product_id, name, symbol) VALUES (1, 'height', '{height}')" () >>
  execute dbConn "INSERT INTO images (url) VALUES ('https://tiozao.co/image2.png')" () >>
  execute dbConn "INSERT INTO dimensions_images (dimension_id, image_id) VALUES (1, 2)" () >>
  execute dbConn "INSERT INTO dimensions (product_id, name, symbol) VALUES (1, 'width', '{width}')" () >>
  execute dbConn "INSERT INTO images (url) VALUES ('https://tiozao.co/image3.png')" () >>
  execute dbConn "INSERT INTO dimensions_images (dimension_id, image_id) VALUES (1, 3)" () >>
  execute dbConn "INSERT INTO dimensions (product_id, name, symbol) VALUES (1, 'depth', '{depth}')" () >>
  execute dbConn "INSERT INTO images (url) VALUES ('https://tiozao.co/image4.png')" () >>
  execute dbConn "INSERT INTO dimensions_images (dimension_id, image_id) VALUES (1, 4)" () >>
  return ()

createUser :: Aeson.FromJSON b => Connection -> WaiSession st b
createUser dbConn = do
  post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
  void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))
  response <- post "/signin" signInRequest
  let rawToken = simpleBody response
  let token = fromJust (Aeson.decode rawToken)
  return token

loggedRequest token method path body = Test.Hspec.Wai.request method path [("Authorization", "Bearer " <> token)] body

createUserRequest :: DBL.ByteString
createUserRequest = "{ \
\    \"name\": \"Andre\", \
\    \"email\": \"email@example.com\", \
\    \"password\": \"somepassword\" \
\}"

signInRequest :: DBL.ByteString
signInRequest = "{ \
\    \"email\": \"email@example.com\", \
\    \"password\": \"somepassword\" \
\}"

productListResponse :: TL.Text
productListResponse = "[{\"description\":\"just a table\",\"dimensions\":[{\"dimensionId\":1,\"images\":[{\"imageId\":2,\"url\":\"https://tiozao.co/image2.png\"},{\"imageId\":3,\"url\":\"https://tiozao.co/image3.png\"},{\"imageId\":4,\"url\":\"https://tiozao.co/image4.png\"}],\"name\":\"height\",\"productId\":1,\"symbol\":\"{height}\"},{\"dimensionId\":2,\"images\":[],\"name\":\"width\",\"productId\":1,\"symbol\":\"{width}\"},{\"dimensionId\":3,\"images\":[],\"name\":\"depth\",\"productId\":1,\"symbol\":\"{depth}\"}],\"images\":[{\"imageId\":1,\"url\":\"https://tiozao.co/image1.png\"}],\"name\":\"table\",\"priceFormula\":\"{height} * {width} * {depth}\",\"productId\":1,\"tag\":\"Product\"}]"

calculatePriceRequest = "{ \
\    \"productId\": 1, \
\    \"dimensionValues\": [ \
\        { \
\            \"dimensionId\": 1, \
\            \"value\": 2.0 \
\        }, \
\        { \
\            \"dimensionId\": 2, \
\            \"value\": 3.0 \
\        }, \
\        { \
\            \"dimensionId\": 3, \
\            \"value\": 4.0 \
\        } \
\    ] \
\}"

calculatePriceResponse = "{\"value\":24}"

suiteSpec :: Connection -> String -> String -> String -> String -> Spec
suiteSpec dbConn host database user password  = do

  with (api host database user password "secret2" 60) $ do
    describe "ProductsHandlerSpec" $ do

      it "Products return invalid Token" $ do
        liftIO (cleanDb dbConn)
        get "/products" `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }

      it "Should return empty product list" $ do
        liftIO (cleanDb dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodGet "/products" "" `shouldRespondWithPredicate` (\response -> "[]" == response, "List not empty")

      it "Should return product list" $ do
        liftIO (cleanDb dbConn)
        liftIO (createProduct dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodGet "/products" "" `shouldRespondWithPredicate` (\response -> productListResponse == response, "Expected response not fulfilled")

      it "Should not calculate product price with wrong request" $ do
        liftIO (cleanDb dbConn)
        liftIO (createProduct dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodPost "/products/calculatePrice" "{\"wrongField\": 1}" `shouldRespondWithPredicate` (\response -> "{\"code\":1,\"message\":\"Invalid Json format\"}" == response, "Expected response not fulfilled")

      it "Should calculate product price" $ do
        liftIO (cleanDb dbConn)
        liftIO (createProduct dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodPost "/products/calculatePrice" calculatePriceRequest `shouldRespondWithPredicate` (\response -> calculatePriceResponse == response, "Expected response not fulfilled")
        
