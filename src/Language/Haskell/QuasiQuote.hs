{-# LANGUAGE QuasiQuotes #-}
module Language.Haskell.QuasiQuote where

import Language.Haskell.TH.Quote

-- A haskell module
hsModule :: QuasiQuoter

hsSC :: QuasiQuoter
