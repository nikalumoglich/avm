{-# LANGUAGE OverloadedStrings #-}

module ImagesHandlerSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Test.Hspec.Wai
import Database.MySQL.Simple
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Text as T
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
import qualified Text.Regex.Pcre2 as Regex
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
  execute dbConn "TRUNCATE TABLE orders" () >>
  execute dbConn "TRUNCATE TABLE orders_dimensions" () >>
  execute dbConn "TRUNCATE TABLE order_interactions" () >>
  execute dbConn "TRUNCATE TABLE order_interactions_images" () >>
  execute dbConn "TRUNCATE TABLE order_interactions_videos" () >>
  return ()

createProduct :: Connection -> IO ()
createProduct dbConn =
  execute dbConn "INSERT INTO products (name, description, price_formula) VALUES ('table', 'just a table', '{height} * {width} * {depth}')" () >>
  execute dbConn "INSERT INTO images (`key`) VALUES ('image1.png')" () >>
  execute dbConn "INSERT INTO products_images (product_id, image_id) VALUES (1, 1)" () >>
  execute dbConn "INSERT INTO dimensions (product_id, name, symbol) VALUES (1, 'height', '{height}')" () >>
  execute dbConn "INSERT INTO images (`key`) VALUES ('image2.png')" () >>
  execute dbConn "INSERT INTO dimensions_images (dimension_id, image_id) VALUES (1, 2)" () >>
  execute dbConn "INSERT INTO dimensions (product_id, name, symbol) VALUES (1, 'width', '{width}')" () >>
  execute dbConn "INSERT INTO images (`key`) VALUES ('image3.png')" () >>
  execute dbConn "INSERT INTO dimensions_images (dimension_id, image_id) VALUES (1, 3)" () >>
  execute dbConn "INSERT INTO dimensions (product_id, name, symbol) VALUES (1, 'depth', '{depth}')" () >>
  execute dbConn "INSERT INTO images (`key`) VALUES ('image4.png')" () >>
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

replaceDateRegex :: T.Text
replaceDateRegex = "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z"

replaceImageUrlRegex :: T.Text
replaceImageUrlRegex = "https:\\/\\/.*?\\.s3\\..*?\\.amazonaws.com\\/(.*?)\\?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=.*?%2Fsa-east-1%2Fs3%2Faws4_request&X-Amz-Date=\\d{8}T\\d{6}Z&X-Amz-Expires=60&X-Amz-SignedHeaders=host&X-Amz-Signature=.{64}"

orderListResponse :: TL.Text
orderListResponse = "[{\"closingDate\":null,\"dimensions\":[{\"dimensionId\":1,\"dimensionName\":\"height\",\"value\":2},{\"dimensionId\":2,\"dimensionName\":\"width\",\"value\":3},{\"dimensionId\":3,\"dimensionName\":\"depth\",\"value\":4}],\"interactions\":[],\"openingDate\":\"\",\"orderId\":1,\"price\":24,\"productId\":1,\"userId\":1}]"

-- orderListResponseWithInteraction :: TL.Text
orderListResponseWithInteraction = "[{\"closingDate\":null,\"dimensions\":[{\"dimensionId\":1,\"dimensionName\":\"height\",\"value\":2},{\"dimensionId\":2,\"dimensionName\":\"width\",\"value\":3},{\"dimensionId\":3,\"dimensionName\":\"depth\",\"value\":4}],\"interactions\":[{\"authorId\":1,\"createdAt\":\"\",\"images\":[{\"imageId\":1,\"url\":\"image1.png\"},{\"imageId\":2,\"url\":\"image2.png\"},{\"imageId\":3,\"url\":\"image3.png\"},{\"imageId\":4,\"url\":\"image4.png\"}],\"interactionId\":1,\"text\":\"test\"}],\"openingDate\":\"\",\"orderId\":1,\"price\":24,\"productId\":1,\"userId\":1}]"

createOrderRequest = "{ \
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

createOrderResponse = "{\"id\":1}"

createOrderInteractionResquest = "{ \
\    \"orderId\": 1, \
\    \"text\": \"test\", \
\    \"imageIds\": [1, 2, 3, 4], \
\    \"videoIds\": [] \
\}"
createOrderInteractionResponse = "{\"id\":1}"

suiteSpec :: Connection -> String -> String -> String -> String -> String -> Spec
suiteSpec dbConn host database user password bucket = do

  with (api host database user password "secret2" 60 bucket) $ do
    describe "ImagesHandlerSpec" $ do

      it "Images return invalid Token" $ do
        liftIO (cleanDb dbConn)
        post "/images" "" `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }

      it "Should return empty order list" $ do
        liftIO (cleanDb dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodGet "/orders" "" `shouldRespondWithPredicate` (\response -> "[]" == response, "List not empty")

      it "Should not create order with invalid JSON" $ do
        liftIO (cleanDb dbConn)
        liftIO (createProduct dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodPost "/orders" "" `shouldRespondWith` "{\"code\":1,\"message\":\"Invalid Json format\"}" { matchStatus = 400 }

      it "Should not create order with invalid token" $ do
        post "/orders" createOrderRequest `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }

      it "Should create order" $ do
        liftIO (cleanDb dbConn)
        liftIO (createProduct dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodPost "/orders" createOrderRequest `shouldRespondWith` createOrderResponse

      it "Should return empty order list" $ do
        liftIO (cleanDb dbConn)
        liftIO (createProduct dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodGet "/orders" "" `shouldRespondWith` "[]"

      it "Should return order list" $ do
        liftIO (cleanDb dbConn)
        liftIO (createProduct dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodPost "/orders" createOrderRequest `shouldRespondWith` createOrderResponse
        loggedRequest token methodGet "/orders" "" `shouldRespondWithPredicate` (\response -> TL.toStrict orderListResponse == Regex.gsub replaceDateRegex "" (TL.toStrict response), "Expected response not fulfilled")

      it "Should not create order interaction with invalid token" $ do
        post "/orders/interactions" createOrderInteractionResquest `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }

      it "Should not create order interaction with invalid JSON" $ do
        liftIO (cleanDb dbConn)
        liftIO (createProduct dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodPost "/orders" createOrderRequest `shouldRespondWith` createOrderResponse
        loggedRequest token methodPost "/orders/interactions" "" `shouldRespondWith` "{\"code\":1,\"message\":\"Invalid Json format\"}" { matchStatus = 400 }
        loggedRequest token methodGet "/orders" "" `shouldRespondWithPredicate` (\response -> TL.toStrict orderListResponse == Regex.gsub replaceDateRegex "" (TL.toStrict response), "Expected response not fulfilled")

      it "Should create order interaction" $ do
        liftIO (cleanDb dbConn)
        liftIO (createProduct dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodPost "/orders" createOrderRequest `shouldRespondWith` createOrderResponse
        loggedRequest token methodPost "/orders/interactions" createOrderInteractionResquest `shouldRespondWith` createOrderInteractionResponse
        loggedRequest token methodGet "/orders" "" `shouldRespondWithPredicate` (\response -> TL.toStrict orderListResponseWithInteraction == Regex.gsub replaceImageUrlRegex "$1" (Regex.gsub replaceDateRegex "" (TL.toStrict response)), "Expected response not fulfilled")