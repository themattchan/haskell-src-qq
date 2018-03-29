module Language.Haskell.QuasiQuote where
import Data.Monoid

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (liftData)
import Language.Haskell.TH.Quote

-- haskell-src-exts
import qualified Language.Haskell.Exts.Parser as HsExts


hsModule :: QuasiQuoter
hsModule = QuasiQuoter quoteExp undefined undefined undefined where
  quoteExp :: String -> TH.Q TH.Exp
  quoteExp s = case HsExts.parseModule s of
    HsExts.ParseFailed srcloc err ->
      fail ("FAIL: " <>err<> " (at "<>show srcloc<>")")
    HsExts.ParseOk expr
      -> liftData expr

hsDecl :: QuasiQuoter
hsDecl = QuasiQuoter quoteExp undefined undefined undefined where
  quoteExp :: String -> TH.Q TH.Exp
  quoteExp s = case HsExts.parseDecl s of
    HsExts.ParseFailed srcloc err ->
      fail ("FAIL: " <>err<> " (at "<>show srcloc<>")")
    HsExts.ParseOk expr
      -> liftData expr

hsExp :: QuasiQuoter
hsExp = QuasiQuoter quoteExp undefined undefined undefined where
  quoteExp :: String -> TH.Q TH.Exp
  quoteExp s = case HsExts.parseExp s of
    HsExts.ParseFailed srcloc err ->
      fail ("FAIL: " <>err<> " (at "<>show srcloc<>")")
    HsExts.ParseOk expr
      -> liftData expr
