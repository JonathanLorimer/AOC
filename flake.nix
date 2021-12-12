{
  description = "Advent of Code";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:
    let utils = flake-utils.lib;
    in
    utils.eachDefaultSystem (system:
    let
      supportedGHCVersion = "8107";
      compilerVersion = "ghc${supportedGHCVersion}";
      pkgs = nixpkgs.legacyPackages.${system};
      hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
        overrides = hfinal: hprev: {
          aoc = hfinal.callCabal2nix "aoc" ./. { };
          streamly = hfinal.callPackage ./nix/streamly.nix {};
          haskell-language-server = hprev.haskell-language-server // {
            override = { supportedGhcVersions = [ supportedGHCVersion ]; };
          };
        };
      };
    in
    rec {
      packages = utils.flattenTree
        { aoc = hsPkgs.aoc; };

      # nix develop
      devShell = hsPkgs.shellFor {
        packages = p: [
          p.aoc
        ];
        buildInputs = with pkgs; [
          hsPkgs.haskell-language-server
          cabal2nix
          haskellPackages.cabal-install
          haskellPackages.ghcid
          haskellPackages.fourmolu
          haskellPackages.cabal-fmt
        ] ++ (builtins.attrValues (import ./scripts.nix { s = pkgs.writeShellScriptBin; }));
      };

      # nix build
      defaultPackage = packages.aoc;
    });
}
