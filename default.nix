{ stdenv, haskellPackages }:
let
  ghc = haskellPackages.ghcWithPackages (p: with p; [
    sqlite-simple
    HandsomeSoup
    feed
    http-conduit
    xml-conduit
  ]);
in
stdenv.mkDerivation {
  name = "nytrss";
  src = ./.;
  buildInputs = [ ghc ];
  buildPhase = ''
    ghc -threaded ./Main.hs -o ./Main -Wall -Werror
  '';
  installPhase = ''
    install -Dm555 ./Main $out/bin/nytrss
  '';
}
