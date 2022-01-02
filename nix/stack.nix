{ pkgs ? import ./nixpkgs-pinned.nix
, ghc ? pkgs.ghc
}:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;

  buildInputs = with pkgs; [
    
  ];

  name = "diagnose";
}
