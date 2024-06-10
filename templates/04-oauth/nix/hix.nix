{pkgs, config, ...}: {
  # name = "project-name";
  compiler-nix-name = "ghc982"; # Version of GHC to use

  # Cross compilation support:
  crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
    p.ghcjs
  ]);

  cabalProjectLocal = ''
    if arch(javascript)
      extra-packages: ghci
  '';

  modules = [
    ({pkgs,config,...}: {
      packages.entropy.package.buildType = pkgs.lib.mkForce "Simple";

      # packages.miso.flags.jsaddle = true;
      packages.hello-spa.flags.jsaddle = true;
    })
    # ({pkgs,lib,config,...}: lib.optionalAttrs (stdenv.hostPlatform.isGhcjs) {
    ({pkgs,lib,config,...}: lib.optionalAttrs (true) {
       # This is necessary because
       # https://github.com/NixOS/nixpkgs/blob/d474c87aff678090f108a23f0b3e521ae0d4e034/pkgs/development/libraries/gnu-config/default.nix
       # is old and doesn't include the "javascript-unknown-ghcjs" tuple. Later
       # versions of gnu-config do, so we'll need to wait until nixpkgs is
       # updated.
       packages.network.components.library.dontUpdateAutotoolsGnuConfigScripts = true;
       packages.network.components.library.preConfigure = ''
         ${pkgs.pkgsBuildHost.autoconf}/bin/autoreconf -i
       '';
    })
  ];

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  shell.tools.hlint = "latest";
  shell.tools.haskell-language-server = "latest";
  shell.tools.hoogle = "latest";
  shell.withHoogle = true;
  shell.packages = ps: builtins.attrValues (pkgs.haskell-nix.haskellLib.selectProjectPackages ps);
  shell.nativeBuildInputs = [ pkgs.buildPackages.cabalWrapped ];
}
