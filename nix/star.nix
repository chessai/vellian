{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "star";
  version = "0.0.0.2";
  src = fetchgit {
    url = "https://github.com/chessai/star.git";
    sha256 = "1zlg06ylr85040xihpwi25l67z8v7sbxl2hws8z472rqh6lx22r0";
    rev = "e1741703c58565d63de9b4db4c3bcaed1150727e";
  };
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/chessai/star#readme";
  description = "*-semirings";
  license = stdenv.lib.licenses.bsd3;
}
