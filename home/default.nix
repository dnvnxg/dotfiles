{ pkgs, lib, ... }:
let
  isDarwin = pkgs.stdenv.isDarwin;
  isLinux = pkgs.stdenv.isLinux;

  gitRepos = [
    { url = "git@github.com:dnvnxg/password-store.git"; dest = ".password-store"; }
  ];

  cloneScript = lib.concatStringsSep "\n" (map (repo: ''
    if [ ! -d "$HOME/${repo.dest}" ]; then
      GIT_SSH_COMMAND="${pkgs.openssh}/bin/ssh" ${pkgs.git}/bin/git clone ${repo.url} "$HOME/${repo.dest}"
    fi
  '') gitRepos);
in {
  home.stateVersion = "24.11";
  home.username = "dxgriego";
  home.homeDirectory = if isDarwin then "/Users/dxgriego" else "/home/dxgriego";

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

  home.activation.cloneRepos = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    export GPG_TTY=$(tty)
    export SSH_AUTH_SOCK=$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)
    ${pkgs.gnupg}/bin/gpgconf --launch gpg-agent
    ${cloneScript}
  '';

  programs.emacs = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    enableZshIntegration = true;
    pinentry.package = if isDarwin then pkgs.pinentry_mac else pkgs.pinentry-gnome3;
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
