{-# LANGUAGE DeriveGeneric #-}

module Controller.ProductController
    ( dimensionId
    , value
    , calculatePrice
    , DimensionValue ( DimensionValue )
    ) where

import qualified Data.Text as T
import Control.Monad.IO.Class
import Language.Haskell.Interpreter
import Control.Monad.Catch
import qualified Data.Map as Map

import qualified Model.Product as Product
import qualified Model.Dimension as Dimension

import GHC.Generics
import qualified Data.Aeson as Aeson

data DimensionValue = DimensionValue
  { dimensionId :: Int
  , value :: Int
  } deriving (Generic)

instance Aeson.FromJSON DimensionValue

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

replaceSymbolsWithValues :: T.Text -> Map.Map Int T.Text -> [DimensionValue] -> T.Text
replaceSymbolsWithValues formula _ [] = formula
replaceSymbolsWithValues formula dimensions (value':values) = replaceSymbolsWithValues replaced dimensions values
    where
        replaced = T.replace (dimensions Map.! dimensionId value') (T.pack $ show (value value')) formula

calculatePrice :: (MonadIO m, Num a, Read a) => Product.Product -> [DimensionValue] -> m a
calculatePrice product dimensionValues = do
    let formula = T.pack (Product.priceFormula product)
    let dimensions = Product.dimensions product
    let mapOfSymbolsByDimensionId = generateDimensionIdSymbolMap dimensions
    let replacedFormula = replaceSymbolsWithValues formula mapOfSymbolsByDimensionId dimensionValues
    liftIO (calculatePriceWith (T.unpack replacedFormula))
                