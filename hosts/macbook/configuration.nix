{ pkgs, ... }: {
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    yubikey-manager
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  security.sudo.extraConfig = ''
    dxgriego ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/darwin-rebuild
  '';

  system.stateVersion = 6;
  nixpkgs.hostPlatform = "aarch64-darwin";
}
