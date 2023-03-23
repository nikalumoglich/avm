{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}

module Handlers.HandlerCommons
    ( handleJsonRequest
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as TL

handleJsonRequest :: Aeson.FromJSON t => ActionT TL.Text IO b -> (t -> ActionT TL.Text IO b) -> ActionT TL.Text IO b
handleJsonRequest errorHandler successHandler = do
    requestBody <- body
    let maybeJson = Aeson.decode requestBody
    case maybeJson of
        Nothing -> errorHandler
        Just json' -> successHandler json'