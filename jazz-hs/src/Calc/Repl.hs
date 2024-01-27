{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-orphans #-}

module Calc.Repl
  ( repl,
  )
where

import qualified Calc.Compile.RunLLVM as Run
import Calc.Compile.ToLLVM
import Calc.Parser
import Calc.Parser.Types
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec
import System.Console.Haskeline
-- import LLVM.Pretty
import Data.Text.Lazy (toStrict)
import Error.Diagnose (WithUnicode(WithUnicode))

instance HasHints Void msg where
  hints _ = mempty

repl :: IO ()
repl = do
  putStrLn "Welcome to llvm-calc"
  putStrLn "Exit with :quit"
  runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          case parseExpr (T.pack input) of
            Left bundle -> do
              Diag.printDiagnostic Diag.stderr Diag.WithUnicode (Diag.TabSize 4) Diag.defaultStyle (fromErrorBundle bundle input)
              loop
            Right expr -> do
              liftIO $ print expr
              let llExp = toLLVM expr
              -- liftIO $ putStrLn (T.unpack $ toStrict $ ppllvm llExp)
              resp <- liftIO $ fmap Run.rrResult (Run.run llExp)
              liftIO $ putStrLn (T.unpack resp)
              loop

-- | turn Megaparsec error + input into a Diagnostic
fromErrorBundle :: ParseErrorType -> String -> Diag.Diagnostic Text
fromErrorBundle bundle input =
  let diag =
        errorDiagnosticFromBundle
          Nothing
          "Parse error on input"
          Nothing
          bundle
   in Diag.addFile diag replFilename input
