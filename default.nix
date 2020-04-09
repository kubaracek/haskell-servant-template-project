{ mkDerivation, aeson, base, bytestring, containers, cryptonite
, either, ekg, ekg-core, elm-bridge, fast-logger, flight-igc
, foreign-store, hashable, hoauth2, http-conduit, katip, microlens
, monad-control, monad-logger, monad-metrics, mtl, persistent
, persistent-postgresql, persistent-template, resource-pool, safe
, servant, servant-auth, servant-auth-server, servant-elm
, servant-js, servant-server, stdenv, text, transformers
, unordered-containers, uri-bytestring, uuid, wai, wai-extra
, wai-middleware-metrics, warp
}:
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers cryptonite either ekg ekg-core
    elm-bridge fast-logger flight-igc foreign-store hashable hoauth2
    http-conduit katip microlens monad-control monad-logger
    monad-metrics mtl persistent persistent-postgresql
    persistent-template resource-pool safe servant servant-elm
    servant-js servant-server text transformers unordered-containers
    uri-bytestring uuid wai wai-extra wai-middleware-metrics warp
  ];
  executableHaskellDepends = [
    aeson base either ekg ekg-core flight-igc hashable hoauth2
    http-conduit microlens monad-logger monad-metrics mtl
    persistent-postgresql safe servant servant-auth servant-auth-server
    servant-elm servant-server uri-bytestring wai-middleware-metrics
    warp
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
