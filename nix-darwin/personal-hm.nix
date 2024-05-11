{ config, pkgs, lib, ... }:
let
  home-directory = builtins.getEnv "HOME";
  tmp-directory = "/tmp";
  dotrcs = import ./dotrcs.nix;
in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "benouattara";
  home.homeDirectory = "/Users/benouattara";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };
  };

  home.file = {
    ".curlrc".text = ''
      capath=${pkgs.cacert}/etc/ssl/certs/
      cacert=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
    '';
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.bash = {
    enable = true;
  };

  programs.git = {
    enable = true;
    #userName  = "Ben O.";
    # userEmail = "benny.ouattara@gmail.com";

    aliases = {
      amend = "commit --amend -C HEAD";
      authors = ''!"${pkgs.git}/bin/git log --pretty=format:%aN''
                + " | ${pkgs.coreutils}/bin/sort" + " | ${pkgs.coreutils}/bin/uniq -c"
                + " | ${pkgs.coreutils}/bin/sort -rn\"";
      b = "branch --color -v";
      ca = "commit --amend";
      clone = "clone --recursive";
      co = "checkout";
      cp = "cherry-pick";
      dc = "diff --cached";
      dh = "diff HEAD";
      ds = "diff --staged";
      undo = "reset --soft HEAD^";
      w = "status -sb";
      wdiff = "diff --color-words";
      l = "log --graph --pretty=format:'%Cred%h%Creset"
          + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
          + " --abbrev-commit --date=relative --show-notes=*";
    };

    extraConfig = {
      pull.rebase = false; # merge

      color = {
        status = "auto";
        diff = "auto";
        branch = "auto";
        interactive = "auto";
        ui = "auto";
        sh = "auto";
      };

      "url \"git@github.com:benny-ouattara/sp-hosts.el.git\"".insteadOf =
        "https://github.com/benny-ouattara/sp-hosts.el.git";

      "url \"git@github.com:\"".insteadOf = "https://github.com/";
    };

    ignores = [ "*.elc" ".clean" ".direnv" "TAGS" "result" "result-*" "tags" ];
  };

  programs.fzf = rec { enable = true; };

  programs.starship = rec { enable = true; };

  programs.zsh = rec {
    enable = true;
    oh-my-zsh = {
      enable = true;
      theme = "gozilla";
    };

    dotDir = ".config/zsh";
    enableCompletion = true;
    autosuggestion.enable = true;

    history = {
      size = 50000;
      save = 500000;
      path = "$HOME/${dotDir}/history";
      ignoreAllDups = true;
      ignoreSpace = true;
      extended = true;
      share = true;
    };

    sessionVariables = {
      ALTERNATE_EDITOR = "${pkgs.vim}/bin/vi";
      LC_CTYPE = "en_US.UTF-8";
      LESS = "-FRSXM";
      PROMPT = "%m %~ $ ";
      PROMPT_DIRTRIM = "2";
      RPROMPT = "";
      WORDCHARS = "";
    };

    shellAliases = {
      r = "ranger";
      ls = "${pkgs.coreutils}/bin/ls --color=auto -alhrt";
      l = "${pkgs.coreutils}/bin/ls --color=auto -alhrt";
      rX = "${pkgs.coreutils}/bin/chmod -R ugo+rX";
      gr = "${pkgs.git}/bin/git rm -r --cached";
      proc = "${pkgs.darwin.ps}/bin/ps axwwww | ${pkgs.gnugrep}/bin/grep -i";
      nstat = "${pkgs.darwin.network_cmds}/bin/netstat -nr -f inet"
              + " | ${pkgs.gnugrep}/bin/egrep -v \"(lo0|vmnet|255\\.255)\""
              + " | ${pkgs.coreutils}/bin/tail -n +5";
      sbcl = "${pkgs.rlwrap}/bin/rlwrap sbcl";
      guile = "${pkgs.rlwrap}/bin/rlwrap guile";
      info = "info --vi-keys";
    };

    profileExtra = ''
      . ${pkgs.z}/share/z.sh
    '';

    initExtra = lib.mkBefore ''
      ZSH_DISABLE_COMPFIX=true

      export PATH=$PATH:$HOME/.jenv/bin:$HOME/.local/bin:$HOME/.emacs.d/bin:/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin:${pkgs.custom-scripts}/bin

      if hash jenv 2>/dev/null; then
         eval "$(jenv init -)"
      fi

      export EDITOR=emacs
      export VISUAL=emacs
      export XDG_CONFIG_HOME="$HOME/.config"
    '';
  };

  programs.ssh = {
    enable = true;
    forwardAgent = true;
    serverAliveInterval = 60;
    hashKnownHosts = true;
    extraConfig = ''
      Host *
        AddKeysToAgent yes
        StrictHostKeyChecking no
        IdentityFile ~/.ssh/id_rsa
        IdentitiesOnly yes
        # LogLevel DEBUG3
    '';

    matchBlocks = {
      keychain = {
        host = "*";
        extraOptions = {
          "UseKeychain" = "yes";
          "AddKeysToAgent" = "yes";
          "IgnoreUnknown" = "UseKeychain";
        };
      };
    };
  };

  xdg = {
    enable = true;
    configHome = "${home-directory}/.config";
    dataHome = "${home-directory}/.local/share";
    cacheHome = "${home-directory}/.cache";
    configFile."mail/mbsyncrc".text = dotrcs.mbsync;
  };
}
