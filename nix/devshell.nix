{ ... }:
{
  perSystem = { pkgs, config, ... }: {
    devShells.default = pkgs.mkShell {
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell
        config.process-compose."service".services.outputs.devShell
      ];

      packages = [];
    };
  };
}
