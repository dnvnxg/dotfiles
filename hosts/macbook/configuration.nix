{ pkgs, ... }: {
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    yubikey-manager
    claude-code
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  security.sudo.extraConfig = ''
    dxgriego ALL=(ALL) NOPASSWD: /nix/store/*/bin/darwin-rebuild
  '';

  homebrew = {
    enable = true;
    casks = [
      "claude"
      "discord"
      "emacs"
      "godot"
      "scroll-reverser"
      "yubico-yubikey-manager"
    ];
  };

  system.stateVersion = 6;
  nixpkgs.hostPlatform = "aarch64-darwin";
}
