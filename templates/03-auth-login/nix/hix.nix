{pkgs, config, ...}: {
  # name = "project-name";
  compiler-nix-name = "ghc982"; # Version of GHC to use

  # Cross compilation support:
  crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
    p.ghcjs
  ]);

  modules = [
    ({pkgs,config,...}: {
      # This is necessary because
      # https://github.com/NixOS/nixpkgs/blob/d474c87aff678090f108a23f0b3e521ae0d4e034/pkgs/development/libraries/gnu-config/default.nix
      # is old and doesn't include the "javascript-unknown-ghcjs" tuple. Later
      # versions of gnu-config do, so we'll need to wait until nixpkgs is
      # updated.
      packages.network.components.library.dontUpdateAutotoolsGnuConfigScripts = true;
      packages.network.components.library.preConfigure = ''
        ${pkgs.pkgsBuildHost.autoconf}/bin/autoreconf -i
      '';

      # packages.hoauth2.components.library.depends = [
      # ];
      packages.hoauth2.components.library.build-tools = [
        # config.packages.ghci
        pkgs.pkgsBuildHost.nodejs
        # config.hsPkgs.ghci
        # config.bootPkgs.ghci
        # config.ghc.package
        # pkgs.pkgsBuildHost.ghcid
        # pkgs.buildPackages.haskell-nix.haskellPackages.ghci
      ];
    })
  ];

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  shell.tools.hlint = "latest";
  shell.tools.haskell-language-server = "latest";
}
