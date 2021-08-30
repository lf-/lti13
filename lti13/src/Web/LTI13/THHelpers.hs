module Web.LTI13.THHelpers where
import Data.Char (toLower)
import Data.List (stripPrefix)

-- | Remove prefix and leading uppercase letter
unPrefix :: String -> String -> String
unPrefix prefix text =
    -- this code is bad on purpose, it will only crash at compile time
    let Just (first:rest) = stripPrefix prefix text
    in toLower first : rest
