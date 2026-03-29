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
          nix-hell = prev.callCabal2nix "nix-hell" ./. { };
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
          buildInputs = with haskellPackages; [
            stack
            cabal-install
            haskell-language-server
          ] ++ runtimeDeps;
        };
      }
    );
}