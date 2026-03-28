{ pkgs, lib, ... }: {
  home.stateVersion = "24.11";
  home.username = "dxgriego";
  home.homeDirectory = "/Users/dxgriego";

  programs.gpg.enable = true;

  programs.git = {
    enable = true;
    settings.user.name = "Donovan Xavier Griego";
    settings.user.email = "dxgriego@gmail.com";
    signing = {
      key = "41EBCEA20BCE406E";
      signByDefault = true;
      format = "openpgp";
    };
  };

  programs.emacs = {
    enable = true;
  };

  home.file.".gnupg/gpg-agent.conf".text = ''
    pinentry-program ${pkgs.pinentry-curses}/bin/pinentry-curses
    enable-ssh-support
  '';

  home.activation.importGpgKey = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    ${pkgs.gnupg}/bin/gpg --import ${../keys/gpg-public-key.asc} 2>/dev/null || true
  '';

  programs.zsh = {
    enable = true;
    initContent = ''
      export GPG_TTY=$(tty)
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
      gpgconf --launch gpg-agent
    '';
  };
}
