{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
      in with pkgs; {
        packages.default = yandere-pic-bot;
        devShells.default = yandere-pic-bot-dev.envFunc { withHoogle = true; };
      }) // {
        overlays.default = final: prev:
          let
            hpkgs = prev.haskellPackages;
            yandere-pic-bot = hpkgs.callCabal2nix "yandere-pic-bot" ./. { };
          in with prev;
          with haskell.lib; {
            yandere-pic-bot = justStaticExecutables yandere-pic-bot;
            yandere-pic-bot-dev = addBuildTools yandere-pic-bot [
              haskell-language-server
              cabal-install
              sqlite
            ];
          };
      };
}
