{ pkgs, lib, ... }: {
  home.stateVersion = "24.11";
  home.username = "dxgriego";
  home.homeDirectory = "/Users/dxgriego";

  programs.gpg = {
    enable = true;
    settings = {
      group = "me = DFEE781D6F6CA99B! 60AFB3575B788937!";
    };
    publicKeys = [
      { source = ../keys/gpg-public-key.asc; trust = 5; }
    ];
  };

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

  programs.password-store = {
    enable = true;
    settings = {};
  };

  home.activation.clonePasswordStore = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [ ! -d "$HOME/.password-store" ]; then
      export GPG_TTY=$(tty)
      export SSH_AUTH_SOCK=$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)
      ${pkgs.gnupg}/bin/gpgconf --launch gpg-agent
      GIT_SSH_COMMAND="${pkgs.openssh}/bin/ssh" ${pkgs.git}/bin/git clone git@github.com:dnvnxg/password-store.git "$HOME/.password-store"
    fi
  '';

  programs.emacs = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    enableZshIntegration = true;
    pinentry.package = pkgs.pinentry_mac;
    sshKeys = [
      "FCFAB8956F7D22D724ADBCDF115E9F19FB57ACAC"
      "C8338759CFB21F019F81EC429D77B0119309B030"
    ];
  };

  programs.zsh = {
    enable = true;
    initContent = ''
      export GPG_TTY=$(tty)
    '';
  };
}
