module PrettyPrintSimple where

import Data.Text.Lazy
import Text.Pretty.Simple

pPrint :: Show a => a -> IO ()
pPrint =
  pPrintOpt
    NoCheckColorTty
    defaultOutputOptionsDarkBg
      { outputOptionsCompact = True,
        outputOptionsCompactParens = True
      }

pShow :: Show a => a -> Text
pShow =
  pShowOpt
    defaultOutputOptionsDarkBg
      { outputOptionsCompact = True,
        outputOptionsCompactParens = True
      }
