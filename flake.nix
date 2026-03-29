{
  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        runtimeDeps = with pkgs; [
          nix
          sops
          age
          ssh-to-age
          systemd
          openssl
          mkcert
        ];

        overlay = final: prev: {
          nix-hell    = prev.callCabal2nix "nix-hell" ./. { };
          microstache = pkgs.haskell.lib.doJailbreak prev.microstache;
          criterion   = pkgs.haskell.lib.doJailbreak prev.criterion;
        };

        haskellPackages = pkgs.haskell.packages.ghc910.extend overlay;

        wrappedBin = pkgs.symlinkJoin {
          name  = "nix-hell";
          paths = [ haskellPackages.nix-hell ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/nix-hell \
              --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}
          '';
        };

      in {
        packages.default = wrappedBin;

        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.nix-hell ];
          buildInputs = [
            pkgs.stack
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
          ] ++ runtimeDeps;
        };
      }
    );
}