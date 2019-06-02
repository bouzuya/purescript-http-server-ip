module Main where

import Prelude

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server as Server
import Bouzuya.HTTP.StatusCode as StatusCode
import Control.Bind (bindFlipped)
import Data.ArrayBuffer.ArrayBuffer as ArrayBuffer
import Data.ArrayBuffer.DataView as DataView
import Data.ArrayBuffer.Typed as TypedArray
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console
import Node.Process as Process

html :: String -> Response
html text =
  { body: stringToUint8Array text
  , headers: [ Tuple.Tuple "Content-Type" "text/html" ]
  , status: StatusCode.status200
  }

stringToUint8Array :: String -> Uint8Array
stringToUint8Array =
  TypedArray.asUint8Array <<< DataView.whole <<< ArrayBuffer.fromString

app :: Request -> Aff Response
app { headers } = do
  -- TODO: request.remoteAddress
  let
    host =
      Maybe.maybe
        "unknown"
        Tuple.snd
        (Foldable.find ((eq "host") <<< Tuple.fst) headers)
  pure (html host)

main :: Effect Unit
main = do
  portMaybe <- map (bindFlipped Int.fromString) (Process.lookupEnv "PORT")
  let config = { hostname: "0.0.0.0", port: Maybe.fromMaybe 8080 portMaybe }
  Server.run config (Console.log "listen") app
