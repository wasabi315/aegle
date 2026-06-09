{
  description = "dependent-type-search";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";

    haskell-flake.url = "github:srid/haskell-flake";

    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";

    emacs.url = "github:nix-community/emacs-overlay";
    emacs.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        ./nix/haskell.nix
        ./nix/service.nix
        ./nix/devshell.nix
      ];

      # Ref: https://discourse.nixos.org/t/how-to-use-overlays-in-a-flake-with-flake-parts/24308
      perSystem = { system, ... }: {
        _module.args.pkgs = import inputs.self.inputs.nixpkgs {
          inherit system;
          overlays = [inputs.emacs.overlays.emacs];
        };
      };
    };
}
