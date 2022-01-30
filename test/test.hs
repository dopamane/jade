import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import qualified Subpar

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  result <- Subpar.syntaxTests
  unless result exitFailure
