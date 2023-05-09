module Handlers.ProductsHandler
    ( listProducts
    , calculatePrice
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import Network.HTTP.Types.Status
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Control.Monad.IO.Class
import Language.Haskell.Interpreter
import Control.Monad.Catch
import qualified Data.Map as Map

import qualified Handlers.HandlerCommons as HandlersCommons
import qualified Model.Product as Product
import qualified Model.Dimension as Dimension
import qualified Transport.CalculatePriceRequest as CalculatePriceRequest
import qualified Transport.CalculatePriceResponse as CalculatePriceResponse

import Errors ( invalidJsonError, invalidSessionError )


invalidJsonResponse :: ActionT TL.Text IO ()
invalidJsonResponse = status badRequest400 >> json invalidJsonError
unauthorizedResponse :: ActionT TL.Text IO ()
unauthorizedResponse = status unauthorized401 >> json invalidSessionError

evals :: (MonadIO m, MonadMask m) => String -> m (Either InterpreterError String)
evals expression = runInterpreter $ setImports ["Prelude"] >> eval expression

calculatePriceWith :: (MonadIO m, MonadMask m, Num b, Read b) => String -> m b
calculatePriceWith formula = do
    evaluationResult <- evals formula
    case evaluationResult of
        Left _ -> return 0
        Right value -> return (read value)

generateDimensionIdSymbolMap :: [Dimension.Dimension] -> Map.Map Int T.Text
generateDimensionIdSymbolMap dimensions = Map.fromList (map (\dimension -> (Dimension.dimensionId dimension, T.pack (Dimension.symbol dimension))) dimensions)

replaceSymbolsWithValues :: T.Text -> Map.Map Int T.Text -> [CalculatePriceRequest.DimensionValue] -> T.Text
replaceSymbolsWithValues formula _ [] = formula
replaceSymbolsWithValues formula dimensions (value:values) = replaceSymbolsWithValues replaced dimensions values
    where
        replaced = T.replace (dimensions Map.! CalculatePriceRequest.dimensionId value) (T.pack $ show (CalculatePriceRequest.value value)) formula



listProducts secret sessionTime conn = HandlersCommons.handleLoggedRequest secret sessionTime conn "userLevel" unauthorizedResponse (\_ -> do
                products <- liftIO (Product.listProducts conn)
                json products
                )

calculatePrice secret sessionTime conn = HandlersCommons.handleLoggedJsonRequest secret sessionTime conn "userLevel" invalidJsonResponse unauthorizedResponse (\calculatePriceRequest _ -> do
                product <- liftIO (Product.getProductById conn (CalculatePriceRequest.productId calculatePriceRequest))
                let formula = T.pack (Product.priceFormula product)
                let dimensions = Product.dimensions product
                let mapOfSymbolsByDimensionId = generateDimensionIdSymbolMap dimensions
                let replacedFormula = replaceSymbolsWithValues formula mapOfSymbolsByDimensionId (CalculatePriceRequest.dimensionValues calculatePriceRequest)
                evaluationResult <- liftIO (calculatePriceWith (T.unpack replacedFormula))
                json CalculatePriceResponse.CalculatePriceResponse { CalculatePriceResponse.value = evaluationResult }
                )