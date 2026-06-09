{ inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];

  perSystem = { self', pkgs, config, ... }: {
    haskellProjects.default = {
      basePackages = pkgs.haskell.packages.ghc912; # Or-patterns and multiline strings!

      packages = {
        Agda.source = "2.8.0"; # Pin Agda version
        pqueue.source = "1.7.0.0"; # 1.5.0.0 has an incompatible constraint on the base package
      };

      settings = {
        hasql-migration = {
          broken = false; # See https://github.com/tvh/hasql-migration/pull/16
          check = false; # Fails on some tests
        };
        unicode-data = {
          check = false; # Fails on some tests
        };
      };

      autoWire = [
        "packages"
        "apps"
        "checks"
      ];
    };

    packages.default = self'.packages.dependent-type-search;
  };
}
