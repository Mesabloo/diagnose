{ pkgs ? import ./nixpkgs-pinned.nix }:

pkgs.mkShell {
  name = "diagnose-dev";

  buildInputs = with pkgs; [
    stack
    (haskell-language-server.override {
      supportedGhcVersions = [ "8107" ];
    })
    ghc
  ];
}
