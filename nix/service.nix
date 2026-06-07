{ inputs, ... }:
{
  perSystem = { pkgs, config, ... }: {
    process-compose."service" = { config, ... }:
      let
        dbName = "dependent-type-search";
      in
      {
        imports = [
          inputs.services-flake.processComposeModules.default
        ];

        services.postgres."pg1" = {
          enable = true;
          package = pkgs.postgresql_18;
          initialDatabases = [
            {
              name = dbName;
            }
          ];
        };

        settings.processes.pgweb =
          let
            pgcfg = config.services.postgres.pg1;
          in
          {
            environment.PGWEB_DATABASE_URL = pgcfg.connectionURI { inherit dbName; };
            command = pkgs.pgweb;
            depends_on."pg1".condition = "process_healthy";
          };

        settings.processes.test = {
          command = pkgs.writeShellApplication {
            name = "pg1-test";
            runtimeInputs = [ config.services.postgres.pg1.package ];
            text = ''
              echo 'SELECT version();' | psql -h 127.0.0.1 ${dbName}
            '';
          };
          depends_on."pg1".condition = "process_healthy";
        };
      };
  };
}
