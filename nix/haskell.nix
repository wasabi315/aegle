{ inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];

  perSystem = { pkgs, config, ... }: {
    haskellProjects.default = {
      basePackages = pkgs.haskell.packages.ghc912;

      settings = {
        hasql-migration = {
          check = false;
          broken = false;
        };
      };

      autoWire = [
        "packages"
        "apps"
        "checks"
      ];
    };
  };
}
