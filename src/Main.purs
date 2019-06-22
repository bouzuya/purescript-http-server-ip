module Main
  ( main
  ) where

import Prelude

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Response as Response
import Bouzuya.HTTP.Server as Server
import Bouzuya.HTTP.StatusCode as StatusCode
import Control.Bind (bindFlipped)
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as Class
import Effect.Console as Console
import Node.Process as Process

html :: String -> Effect Response
html =
  Response.response
    StatusCode.status200
    [ Tuple.Tuple "Content-Type" "text/html" ]

app :: Request -> Aff Response
app { headers } = do
  -- TODO: request.remoteAddress
  let
    host =
      Maybe.maybe
        "unknown"
        Tuple.snd
        (Foldable.find ((eq "host") <<< Tuple.fst) headers)
  Class.liftEffect (html host)

main :: Effect Unit
main = do
  portMaybe <- map (bindFlipped Int.fromString) (Process.lookupEnv "PORT")
  let config = { host: "0.0.0.0", port: Maybe.fromMaybe 8080 portMaybe }
  Server.run
    config
    (\{ host, port } ->
      Console.log ("listen: http://" <> host <> ":" <> (show port)))
    app
