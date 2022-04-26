import
  (builtins.fetchTarball {
    name = "nixpkgs-pinned";
    url = "https://github.com/nixos/nixpkgs/archive/a7ecde854aee5c4c7cd6177f54a99d2c1ff28a31.tar.gz";
    # Use `nix-prefetch-url --unpack <url>`
    sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  })
{ }

