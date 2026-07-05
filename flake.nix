# ./flake.nix
{
  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Pinned process contracts. NixHell primitives shell out to
        # these; wrapping them means the language's semantics do not
        # depend on host tool versions. systemd is the one deliberate
        # skew risk: a pinned systemctl talking to the host's PID 1,
        # accepted so systemd-unit deployments with barren PATH work.
        runtimeDeps = with pkgs; [
          nix
          sops
          age
          ssh-to-age
          systemd
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
          buildInputs = [
            wrappedBin
            pkgs.zlib
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
          ] ++ runtimeDeps;
        };
      }
    );
}