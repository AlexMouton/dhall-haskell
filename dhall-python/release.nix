let
  default = (import ./default.nix);

in
  { dhall-python = default.all;

    inherit (default) tarball;
  }
