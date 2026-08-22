{
  description = "Donovan's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-26.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
    # Pin Homebrew to a release new enough to recognize macOS 27 (golden_gate);
    # the version vendored by nix-homebrew tops out at macOS 26 (tahoe).
    brew-src = {
      url = "github:Homebrew/brew/6.0.2";
      flake = false;
    };
    nix-homebrew.inputs.brew-src.follows = "brew-src";
  };

  outputs = { self, nixpkgs, nix-darwin, home-manager, nix-homebrew, sops-nix, ... }:
  let
    systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

    mkDarwin = { hostname, username, extraModules ? [] }: nix-darwin.lib.darwinSystem {
      specialArgs = { inherit username; };
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
          home-manager.sharedModules = [ sops-nix.homeManagerModules.sops ];
          users.users.${username}.home = "/Users/${username}";
          system.primaryUser = username;
          networking.computerName = hostname;
          networking.hostName = hostname;
        }
      ] ++ extraModules;
    };

    # Home config for a host someone else administers: user-level tooling only.
    mkGuestHome = system: home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};
      extraSpecialArgs = { username = "dxgriego"; };
      modules = [
        sops-nix.homeManagerModules.sops
        ./home/default.nix
        {
          custom.personalMachine = false;
          custom.gitRepos = [ ];
        }
      ];
    };

    mkNixos = { hostname, username, system ? "x86_64-linux", extraModules ? [] }: nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = { inherit username; };
      modules = [
        ./hosts/common/nixos.nix
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.${username} = import ./home/default.nix;
          home-manager.extraSpecialArgs = { inherit username; };
          home-manager.sharedModules = [ sops-nix.homeManagerModules.sops ];
          users.users.${username} = {
            isNormalUser = true;
            home = "/home/${username}";
            extraGroups = [ "wheel" ];
          };
          networking.hostName = hostname;
        }
      ] ++ extraModules;
    };
  in {
    # Consumable by other flakes (e.g. a future servers repo):
    #   home-manager.users.dxgriego = inputs.dotfiles.homeModules.default;
    homeModules.default = ./home/default.nix;

    # Standalone, for a box I do not administer:
    #   home-manager switch --flake github:dnvnxg/dotfiles#dxgriego
    homeConfigurations = {
      "dxgriego" = mkGuestHome "x86_64-linux";
      "dxgriego-aarch64" = mkGuestHome "aarch64-linux";
    };

    # `nix run .#enroll-host` - add this machine as an age recipient so it can
    # decrypt secrets unattended. Needs a YubiKey once, then never again.
    apps = forAllSystems (pkgs: {
      enroll-host = {
        type = "app";
        program = "${pkgs.writeShellApplication {
          name = "enroll-host";
          runtimeInputs = with pkgs; [ age sops git gnupg python3 ];
          text = builtins.readFile ./scripts/enroll-host.sh;
        }}/bin/enroll-host";
      };
    });

    darwinConfigurations."Donovans-MacBook-Pro" = mkDarwin {
      hostname = "Donovans-MacBook-Pro";
      username = "dxgriego";
      extraModules = [ ./hosts/macbook/configuration.nix ];
    };
  };
}
