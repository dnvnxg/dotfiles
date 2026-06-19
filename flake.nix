{
  description = "Donovan's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-26.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
    # Pin Homebrew to a release new enough to recognize macOS 27 (golden_gate);
    # the version vendored by nix-homebrew tops out at macOS 26 (tahoe).
    brew-src = {
      url = "github:Homebrew/brew/6.0.2";
      flake = false;
    };
    nix-homebrew.inputs.brew-src.follows = "brew-src";
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
          networking.computerName = hostname;
          networking.hostName = hostname;
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
          networking.hostName = hostname;
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
