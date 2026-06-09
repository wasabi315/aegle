{
  description = "dependent-type-search";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05"; # nixpkgs-unstable fails to build emacs on my laptop
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";

    haskell-flake.url = "github:srid/haskell-flake";

    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        ./nix/haskell.nix
        ./nix/service.nix
        ./nix/devshell.nix
      ];
    };
}
