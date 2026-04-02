{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    yubikey-manager
    claude-code
  ];

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
