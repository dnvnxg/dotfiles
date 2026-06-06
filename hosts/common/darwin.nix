{ config, ... }: {
  imports = [ ./nix.nix ];

  security.sudo.extraConfig = ''
    ${config.system.primaryUser} ALL=(ALL) NOPASSWD: /nix/store/*/bin/darwin-rebuild
  '';

  nix.gc = {
    automatic = true;
    interval = { Weekday = 0; Hour = 3; Minute = 0; };
    options = "--delete-older-than 30d";
  };

  nix.optimise = {
    automatic = true;
    interval = { Weekday = 0; Hour = 4; Minute = 0; };
  };
}
