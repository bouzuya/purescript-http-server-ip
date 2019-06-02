module Main where

import Prelude

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server as Server
import Bouzuya.HTTP.StatusCode as StatusCode
import Data.ArrayBuffer.ArrayBuffer as ArrayBuffer
import Data.ArrayBuffer.DataView as DataView
import Data.ArrayBuffer.Typed as TypedArray
import Data.ArrayBuffer.Types (Uint8Array)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console

stringToUint8Array :: String -> Uint8Array
stringToUint8Array =
  TypedArray.asUint8Array <<< DataView.whole <<< ArrayBuffer.fromString

app :: Request -> Aff Response
app request = do
  let body = stringToUint8Array "OK"
  pure { status: StatusCode.status200, headers: mempty, body }

main :: Effect Unit
main = do
  let config = { hostname: "0.0.0.0", port: 8080 }
  Server.run config (Console.log "listen") app
