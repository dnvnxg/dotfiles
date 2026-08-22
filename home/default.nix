{ config, pkgs, lib, username, gpgIdentity, ... }:
let
  isDarwin = pkgs.stdenv.isDarwin;
  isLinux = pkgs.stdenv.isLinux;
  gpgKeys = import "${gpgIdentity}/keys.nix";
  homeDir = if isDarwin then "/Users/${username}" else "/home/${username}";
  cfg = config.custom;

  cloneScript = lib.concatStringsSep "\n" (map (repo: ''
    if [ ! -d "$HOME/${repo.dest}" ]; then
      $DRY_RUN_CMD GIT_SSH_COMMAND="${pkgs.openssh}/bin/ssh" ${pkgs.git}/bin/git clone ${repo.url} "$HOME/${repo.dest}"
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
    personalMachine = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Whether this is a machine I own and administer.

        Set false when dropping this config onto a host someone else runs.
        That restricts it to user-level tooling: it will not manage
        authorized_keys, clone personal repos, decrypt secrets, or assume a
        graphical session for pinentry.
      '';
    };

    gitRepos = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          url = lib.mkOption { type = lib.types.str; };
          dest = lib.mkOption { type = lib.types.str; };
        };
      });
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
      publicKeys = [
        { source = "${gpgIdentity}/public-key.asc"; trust = 5; }
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
      settings = { }; # explicit empty: keep pass on ~/.password-store, not the legacy XDG default
    };

    home.activation.configureDotfilesRemote =
      lib.mkIf cfg.personalMachine (lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if [ -d "$HOME/dotfiles/.git" ]; then
        $DRY_RUN_CMD ${pkgs.git}/bin/git -C "$HOME/dotfiles" remote set-url origin https://github.com/dnvnxg/dotfiles.git
        $DRY_RUN_CMD ${pkgs.git}/bin/git -C "$HOME/dotfiles" remote set-url --push origin git@github.com:dnvnxg/dotfiles.git
      fi
    '');

    home.activation.cloneRepos =
      lib.mkIf (cfg.personalMachine && cfg.gitRepos != [ ]) (lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      GPG_TTY=$(tty 2>/dev/null) || true
      export GPG_TTY
      export SSH_AUTH_SOCK=$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)
      ${pkgs.gnupg}/bin/gpgconf --launch gpg-agent
      ${cloneScript}
    '');

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      enableZshIntegration = true;
      pinentry.package =
        if isDarwin then pkgs.pinentry_mac
        else if cfg.personalMachine then pkgs.pinentry-gnome3
        else pkgs.pinentry-curses;
      sshKeys = gpgKeys.sshKeygrips;
    };

    # Only manage authorized_keys on machines I administer - clobbering it on
    # someone else's host can lock me out of the session I am connected over.
    home.file.".ssh/authorized_keys" = lib.mkIf cfg.personalMachine {
      text = lib.concatStringsSep "\n" gpgKeys.sshPublicKeys + "\n";
    };

    home.packages = [ pkgs.sops ];

    # Secrets are committed to this repo encrypted to the OpenPGP key in keys/.
    # Decryption happens in a launchd agent (darwin) / user service (linux) at
    # login and on activation, so a YubiKey must be inserted at that point.
    sops = lib.mkIf cfg.personalMachine {
      defaultSopsFile = ../secrets/secrets.yaml;
      # Per-machine age key: lets this host decrypt at login with no PIN.
      # Generated on first activation; authorized via `nix run .#enroll-host`.
      #
      # sops-install-secrets accepts exactly one key source, so this cannot
      # also list gnupg.home. The OpenPGP key stays a recipient in .sops.yaml
      # regardless, so the YubiKey still authors and edits secrets via the
      # sops CLI - it just isn't what unattended decryption uses.
      # Until this host is enrolled, decryption here fails and secrets are
      # simply absent.
      age = {
        keyFile = "${homeDir}/.config/sops/age/keys.txt";
        generateKey = true;
      };
      secrets.openrouter_api_key = { };
    };

    programs.zsh = {
      enable = true;
      # Login shells only: decrypt once, let every child shell inherit.
      # Adopted from the pre-home-manager ~/.zprofile left by the Homebrew
      # installer and pipx; that file blocked activation while unmanaged.
      profileExtra = ''
        ${lib.optionalString isDarwin ''
          eval "$(/opt/homebrew/bin/brew shellenv)"
        ''}
        export PATH="$PATH:$HOME/.local/bin"

        _sops_secrets="$HOME/.config/sops-nix/secrets"
        [ -r "$_sops_secrets/openrouter_api_key" ] \
          && export OPENROUTER_API_KEY="$(cat "$_sops_secrets/openrouter_api_key")"
        unset _sops_secrets
      '';
      initContent = ''
        [ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"
      '';
    };
  };
}
