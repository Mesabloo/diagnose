import (builtins.fetchTarball {
  name = "nixpkgs-pinned";
  url = "https://github.com/nixos/nixpkgs/archive/43cdc5b364511eabdcad9fde639777ffd9e5bab1.tar.gz";
  # Use `nix-prefetch-url --unpack <url>`
  sha256 = "1wrd5rrpa2fcy6q9193vgihiqmqc686vmwqyn24cb0fk1a37m2g4";
}) {}
