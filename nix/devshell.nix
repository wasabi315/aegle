{ ... }:
{
  perSystem = { pkgs, config, ... }: {
    devShells.default = pkgs.mkShell {
      inputsFrom = [
        config.process-compose."service".services.outputs.devShell
      ];
      packages = [];
    };
  };
}
