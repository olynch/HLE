{ mkDerivation, base, containers, hpack, linear, microlens-platform
, mtl, protolude, stdenv, text, transformers
}:
mkDerivation {
  pname = "HLE";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base containers linear microlens-platform mtl protolude text
    transformers
  ];
  preConfigure = "hpack";
  license = stdenv.lib.licenses.bsd3;
}
