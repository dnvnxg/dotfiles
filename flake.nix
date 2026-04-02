{
  description = "Donovan's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
  };

  outputs = { self, nixpkgs, nix-darwin, home-manager, nix-homebrew, ... }:
  let
    mkDarwin = { hostname, username, extraModules ? [] }: nix-darwin.lib.darwinSystem {
      modules = [
        ./hosts/common/darwin.nix
        home-manager.darwinModules.home-manager
        nix-homebrew.darwinModules.nix-homebrew
        {
          nix-homebrew = {
            enable = true;
            user = username;
            autoMigrate = true;
          };
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.${username} = import ./home/default.nix;
          home-manager.extraSpecialArgs = { inherit username; };
          users.users.${username}.home = "/Users/${username}";
          system.primaryUser = username;
        }
      ] ++ extraModules;
    };

    mkNixos = { hostname, username, system ? "x86_64-linux", extraModules ? [] }: nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        ./hosts/common/nixos.nix
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.${username} = import ./home/default.nix;
          home-manager.extraSpecialArgs = { inherit username; };
          users.users.${username} = {
            isNormalUser = true;
            home = "/home/${username}";
            extraGroups = [ "wheel" ];
          };
          system.primaryUser = username;
        }
      ] ++ extraModules;
    };
  in {
    darwinConfigurations."Donovans-MacBook-Pro" = mkDarwin {
      hostname = "Donovans-MacBook-Pro";
      username = "dxgriego";
      extraModules = [ ./hosts/macbook/configuration.nix ];
    };
  };
}
