{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    yubikey-manager
  ];

  homebrew = {
    enable = true;
    brews = [
      "opencode"
    ];
    casks = [
      "claude"
      "discord"
      "emacs-app"
      "godot"
      "opencode-desktop"
      "scroll-reverser"
      "yubico-yubikey-manager"
    ];
  };

  system.stateVersion = 6;
  nixpkgs.hostPlatform = "aarch64-darwin";
}
