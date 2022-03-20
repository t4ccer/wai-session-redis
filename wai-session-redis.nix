{ mkDerivation, base, bytestring, cereal, data-default, hedis
, hpack, hspec, http-types, lib, vault, wai, wai-session, warp
}:
mkDerivation {
  pname = "wai-session-redis";
  version = "0.1.0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cereal data-default hedis vault wai wai-session
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring cereal data-default hedis http-types vault wai
    wai-session warp
  ];
  testHaskellDepends = [
    base bytestring cereal data-default hedis hspec vault wai
    wai-session
  ];
  prePatch = "hpack";
  homepage = "https://github.com/t4ccer/wai-session-redis#readme";
  description = "Simple Redis backed wai-session backend";
  license = lib.licenses.bsd3;
}
