{ pkgs, ... }: {
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    gnupg
    yubikey-manager
    pass
    git
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  system.stateVersion = 6;
  nixpkgs.hostPlatform = "aarch64-darwin";
}
