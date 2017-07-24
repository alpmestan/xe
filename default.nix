{ mkDerivation, base, lens, optparse-applicative, stdenv
, taggy-lens, text, time, wreq
}:
mkDerivation {
  pname = "xe";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base lens optparse-applicative taggy-lens text time wreq
  ];
  description = "Get FX rates from xe.com";
  license = stdenv.lib.licenses.bsd3;
}
