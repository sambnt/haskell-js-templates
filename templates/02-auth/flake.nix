{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: _prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                # uncomment with your current system for `nix flake show --allow-import-from-derivation` to work:
                evalSystem = "x86_64-linux";
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in nixpkgs.lib.recursiveUpdate flake {
        legacyPackages = pkgs;
        packages = {
          hello-frontend-js = flake.packages."javascript-unknown-ghcjs:hello-spa:exe:hello-frontend".overrideDerivation (drv: {
            # The default builder doesn't export the generated html/javascript
            postInstall = ''
              cp dist/build/${drv.identifier.component-name}/${drv.exeName}.jsexe/all.js $out/bin/
              cp dist/build/${drv.identifier.component-name}/${drv.exeName}.jsexe/index.html $out/bin/
            '';
          });
        };
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
