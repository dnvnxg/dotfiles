{ config, ... }: {
  imports = [ ./nix.nix ];

  security.sudo.extraRules = [{
    users = [ config.system.primaryUser ];
    commands = [{
      command = "/run/current-system/sw/bin/nixos-rebuild";
      options = [ "NOPASSWD" ];
    }];
  }];
}
