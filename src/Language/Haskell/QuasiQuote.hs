{-# LANGUAGE LambdaCase #-}
module Language.Haskell.QuasiQuote where
import Data.Monoid
import Data.Data (Data)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (liftData)
import Language.Haskell.TH.Quote

-- haskell-src-exts
import qualified Language.Haskell.Exts.Parser as HsExts

mkQuasiQuoter :: Data a => (String -> HsExts.ParseResult a) -> QuasiQuoter
mkQuasiQuoter f =  QuasiQuoter (convert f) undefined undefined undefined
  where
    convert = fmap $ \case
        HsExts.ParseFailed srcloc err ->
          fail ("FAIL: " <>err<> " (at "<>show srcloc<>")")
        HsExts.ParseOk expr
          -> liftData expr

hsModule :: QuasiQuoter
hsModule = mkQuasiQuoter HsExts.parseModule

hsDecl :: QuasiQuoter
hsDecl = mkQuasiQuoter HsExts.parseDecl

hsExp :: QuasiQuoter
hsExp = mkQuasiQuoter HsExts.parseExp
