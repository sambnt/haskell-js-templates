module Config.CORS where

import Data.Text (Text)
import Options.Applicative (Parser, strOption, some, long, short, metavar, help)

data ConfigCORS
  = ConfigCORS { cfgCORSOrigins :: [Text]
               }
  deriving (Eq, Show)

pConfigCORS :: Parser ConfigCORS
pConfigCORS = ConfigCORS
  <$> some (strOption ( long "allow-origin"
                        <> short 'o'
                        <> metavar "URL"
                        <> help "Origins to allow requests from"
                      ))
