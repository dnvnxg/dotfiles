{ config, pkgs, lib, username, ... }:
let
  isDarwin = pkgs.stdenv.isDarwin;
  isLinux = pkgs.stdenv.isLinux;
  gpgKeys = import ../keys/gpg-keys.nix;
  homeDir = if isDarwin then "/Users/${username}" else "/home/${username}";
  cfg = config.custom;

  cloneScript = lib.concatStringsSep "\n" (map (repo: ''
    if [ ! -d "$HOME/${repo.dest}" ]; then
      GIT_SSH_COMMAND="${pkgs.openssh}/bin/ssh" ${pkgs.git}/bin/git clone ${repo.url} "$HOME/${repo.dest}"
    fi
  '') cfg.gitRepos);
in {
  options.custom = {
    gitName = lib.mkOption {
      type = lib.types.str;
      default = "Donovan Xavier Griego";
    };
    gitEmail = lib.mkOption {
      type = lib.types.str;
      default = "dxgriego@gmail.com";
    };
    gitRepos = lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      default = [
        { url = "git@github.com:dnvnxg/password-store.git"; dest = ".password-store"; }
        { url = "git@github.com:dnvnxg/org.git"; dest = "org"; }
      ];
    };
  };

  config = {
  home.stateVersion = "24.11";
  home.username = username;
  home.homeDirectory = homeDir;

  programs.gpg = {
    enable = true;
    settings = {
      group = "me = ${lib.concatMapStringsSep " " (k: "${k}!") gpgKeys.encryptionSubkeys}";
    };
    publicKeys = [
      { source = ../keys/gpg-public-key.asc; trust = 5; }
    ];
  };

  programs.git = {
    enable = true;
    settings.user.name = cfg.gitName;
    settings.user.email = cfg.gitEmail;
    signing = {
      key = gpgKeys.signingKey;
      signByDefault = true;
      format = "openpgp";
    };
  };

  programs.password-store = {
    enable = lib.any (r: r.dest == ".password-store") cfg.gitRepos;
    settings = {};
  };

  home.packages = [ pkgs.git ];

  home.file.".local/bin/dotfiles-sync" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      set -euo pipefail

      export GPG_TTY=$(tty) 2>/dev/null || true
      export SSH_AUTH_SOCK=$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)

      DOTFILES="$HOME/dotfiles"
      REBUILD_CMD="${if isDarwin then "sudo darwin-rebuild switch" else "sudo nixos-rebuild switch"}"

      cd "$DOTFILES"
      ${pkgs.git}/bin/git fetch origin

      LOCAL=$(${pkgs.git}/bin/git rev-parse HEAD)
      REMOTE=$(${pkgs.git}/bin/git rev-parse @{u})

      if [ "$LOCAL" != "$REMOTE" ]; then
        ${pkgs.git}/bin/git pull --ff-only
        $REBUILD_CMD --flake "$DOTFILES"
      fi
    '';
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
    sshKeys = gpgKeys.sshKeygrips;
  };

  home.file.".ssh/authorized_keys".text = lib.concatStringsSep "\n" gpgKeys.sshPublicKeys + "\n";

  launchd.agents.dotfiles-sync = lib.mkIf isDarwin {
    enable = true;
    config = {
      ProgramArguments = [ "${homeDir}/.local/bin/dotfiles-sync" ];
      StartInterval = 300;
      StandardOutPath = "${homeDir}/.local/share/dotfiles-sync/stdout.log";
      StandardErrorPath = "${homeDir}/.local/share/dotfiles-sync/stderr.log";
    };
  };

  systemd.user.services.dotfiles-sync = lib.mkIf isLinux {
    Unit.Description = "Sync and rebuild dotfiles";
    Service = {
      Type = "oneshot";
      ExecStart = "%h/.local/bin/dotfiles-sync";
    };
  };

  systemd.user.timers.dotfiles-sync = lib.mkIf isLinux {
    Unit.Description = "Sync and rebuild dotfiles timer";
    Timer = {
      OnBootSec = "5m";
      OnUnitActiveSec = "5m";
    };
    Install.WantedBy = [ "timers.target" ];
  };

  programs.zsh = {
    enable = true;
    initContent = ''
      export GPG_TTY=$(tty)
    '';
  };
  }; # close config
}
