import
  (builtins.fetchTarball {
    name = "nixpkgs-pinned";
    url = "https://github.com/nixos/nixpkgs/archive/7b06206fa24198912cea58de690aa4943f238fbf.tar.gz";
    # Use `nix-prefetch-url --unpack <url>`
    sha256 = "0q53nmwj96gf9q0y6krbf7969w54ymni9wfrca25sqfdzjzk65bm";
  })
{ }

