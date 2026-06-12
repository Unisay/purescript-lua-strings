{
  description = "Purescript-Lua Flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixos-unstable";
    easyps = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    pslua.url = "github:Unisay/purescript-lua";
  };

  outputs = { self, nixpkgs, flake-utils, easyps, pslua }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        p = nixpkgs.legacyPackages.${system};
        e = import easyps { pkgs = p; };
        l = p.lua51Packages;
      in {
        devShell = p.mkShell {
          buildInputs = [
              p.dhall
              l.lua
              l.luacheck
              p.luaformatter
              p.nixfmt
              pslua.packages.${system}.default
              e.purs-0_15_15
              e.spago
              p.treefmt
            ];
        };
      });
}

