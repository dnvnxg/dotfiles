{ ... }: {
  homebrew = {
    enable = true;
    brews = [
      # ykman CLI. The nixpkgs build crashes on this macOS (libffi trampoline
      # assertion in python-cffi), so the CLI comes from Homebrew instead.
      "ykman"
      "opencode"
    ];
    casks = [
      "claude"
      "discord"
      "emacs-app"
      "godot"
      "opencode-desktop"
      "scroll-reverser"
      "yubico-yubikey-manager" # GUI
    ];
  };

  system.stateVersion = 6;
  nixpkgs.hostPlatform = "aarch64-darwin";
}
