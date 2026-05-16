{ config, pkgs, lib, ... }:
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

  home.sessionPath = [
    "$HOME/.local/bin"
    "$HOME/.emacs.d/bin"
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home = {
    file.".curlrc".text = ''
      capath=${pkgs.cacert}/etc/ssl/certs/
      cacert=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
    '';
  };

  programs.aichat = {
    enable = true;
    settings = {
      model = "ollama:deepseek-r1:1.5b";
      clients = [
        {
          type = "openai-compatible";
          name = "ollama";
          api_base = "http://otter:11434/v1";
          models = [
            {
              name = "deepseek-r1:32b";
              supports_function_calling = true;
              supports_vision = false;
            }
          ];
        }
      ];
    };
  };

  # programs.aider-chat = {
  #   enable = true;
  #   settings = {
  #     architect = true;
  #     auto-accept-architect = false;
  #     cache-prompts = true;
  #     check-model-accepts-settings = false;
  #     dark-mode = true;
  #     dirty-commits = false;
  #     lint = true;
  #     show-model-warnings = false;
  #     verify-ssl = false;
  #   };
  # };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.bash = {
    enable = true;
  };

  programs.git = {
    enable = true;

    settings = {
      user = {
        name = "Ben A.";
        email = "ben.ouattara@proton.me";
      };
      alias = {
        amend = "commit --amend -C HEAD";
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
        l = "log --graph --pretty=format:'%Cred%h%Creset"
            + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
            + " --abbrev-commit --date=relative --show-notes=*";
      };

      gpg.format = "openpgp";
      gpg.openpgp.program = "gpg";
      pull.rebase = true;

      color = {
        status = "auto";
        diff = "auto";
        branch = "auto";
        interactive = "auto";
        ui = "auto";
        sh = "auto";
      };
    };

    ignores = [ "*.elc" ".clean" ".direnv" ".DS_Store" ".env" ".env.*" "node_modules/" "TAGS" "result" "result-*" "tags" ];
  };

  programs.fzf = {
    enable = true;
    defaultOptions = [
      "--height 40%"
      "--layout=reverse"
      "--border"
      "--bind 'ctrl-n:down,ctrl-p:up'"
      "--preview 'bat --color=always --style=numbers --line-range=:500 {}'"
    ];

    # File search: Add the preview here!
    fileWidgetOptions = [
      "--preview 'bat --color=always --style=numbers --line-range=:500 {}'"
    ];

    # Directory search: No preview (or use 'eza' if you want to see folder contents)
    changeDirWidgetOptions = [
      "--preview 'eza --tree --level 2 --color=always {} | head -200'"
    ];
  };

  programs.nix-index = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
    options = [
      "--cmd j" # This lets you use 'j' instead of 'z' if you prefer
      "--hook prompt"
    ];
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    dotDir = "${config.xdg.configHome}/zsh";

    oh-my-zsh = {
      enable = false;
      theme = "gozilla";
    };

    history = {
      size = 50000;
      save = 500000;
      path = "${config.xdg.configHome}/zsh/history";
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
      aider="OLLAMA_API_BASE=\"http://otter:11434\" aider";
      snore="sudo -v; while true; do sudo -n true; sleep 60; kill -0 \"$$\" || exit; done 2>/dev/null &";
      r = "ranger";
      lg = "lazygit";
      ask = "aichat";
      think = "aichat --model ollama:mistral:latest";
      reload = "exec $SHELL -l";
      z = "__zoxide_zi";
      ls = "eza";
      zstats = "zoxide query -l"; # See your most visited paths
      otter-on = "ssh -M -S ~/.ssh/master-ben@otter:22 -fN otter";
      otter-off = "ssh -S ~/.ssh/master-ben@otter:22 -O exit otter";
      sbcl = "${pkgs.rlwrap}/bin/rlwrap sbcl";
      guile = "${pkgs.rlwrap}/bin/rlwrap guile";
      info = "info --vi-keys";
      oc-on="ssh -f oc-tunnel";
      oc-off="pkill -f oc-tunnel";
      oc-dash="ssh -f oc-tunnel && open http://localhost:18789";
      # ls = "${pkgs.coreutils}/bin/ls --color=auto -alhrt";
      # l = "${pkgs.coreutils}/bin/ls --color=auto -alhrt";
      # rX = "${pkgs.coreutils}/bin/chmod -R ugo+rX";
      # gr = "${pkgs.git}/bin/git rm -r --cached";
      # proc = "${pkgs.darwin.ps}/bin/ps axwwww | ${pkgs.gnugrep}/bin/grep -i";
      # nstat = "${pkgs.darwin.network_cmds}/bin/netstat -nr -f inet"
      #         + " | ${pkgs.gnugrep}/bin/egrep -v \"(lo0|vmnet|255\\.255)\""
      #         + " | ${pkgs.coreutils}/bin/tail -n +5";
    };

    initContent = lib.mkBefore ''
      # Prevent duplicate entries in PATH
      typeset -U path

      ZSH_DISABLE_COMPFIX=true

      # export PATH=/etc/profiles/per-user/$USER/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:$HOME/.jenv/bin:$HOME/.local/bin:$HOME/.emacs.d/bin:${pkgs.custom-scripts}/bin:$PATH
      export PATH=$PATH:${config.home.homeDirectory}/nix-darwin/scripts

      export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
      # source /nix/var/nix/profiles/per-user/$USER/profile/etc/profile.d/nix.sh

      if hash jenv 2>/dev/null; then
         eval "$(jenv init -)"
      fi

      export EDITOR=emacs
      export VISUAL=emacs

      source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh

      # Suppress the 'no such file' error during startup
      # This ignores the specific brew error while we fix the underlying symlink
      autoload -Uz compinit
      compinit -i -u # -i ignores insecure files, -u ignores the 'site-functions' check if empty

      # Re-bind the widgets to available Ctrl keys
      # Ctrl-F: Find Files (Replacing Ctrl-T)
      # Ctrl-G: Get History (Replacing Ctrl-R)
      # Ctrl-B: Browse/CD (Replacing Alt-C)

      source "${pkgs.fzf}/share/fzf/completion.zsh"
      source "${pkgs.fzf}/share/fzf/key-bindings.zsh"
      bindkey '^F' fzf-file-widget      # Ctrl + F
      bindkey '^G' fzf-history-widget   # Ctrl + G
      bindkey '^B' fzf-cd-widget        # Ctrl + B

      # Ctrl+O: open lazygit
      function _lazygit_widget() { lazygit; zle reset-prompt }
      zle -N _lazygit_widget
      bindkey '^O' _lazygit_widget

      # Ctrl+X: open broot and cd into selected dir
      function _broot_widget() {
        local dir=$(broot --only-folders --cmd ':print_path')
        if [ -n "$dir" ]; then cd "$dir"; fi
        zle reset-prompt
      }
      zle -N _broot_widget
      bindkey '^X' _broot_widget
      '';
  };

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    matchBlocks = {
      "*" = {
        # Fix: Move these here from the top level
        controlMaster = "auto";
        controlPath = "~/.ssh/master-%r@%h:%p";
        controlPersist = "yes";
        
        # Your existing global settings
        hashKnownHosts = true;
        forwardAgent = true;
        serverAliveInterval = 60;
        
        extraOptions = {
          "UseKeychain" = "no";
          "AddKeysToAgent" = "no";
        };
      };

      "github.com" = {
        controlMaster = "no";
      };

      "otter" = {
        user = "ben";
        compression = true;
        extraOptions = { 
          "ServerAliveInterval" = "15";
          "ServerAliveCountMax" = "3";
        };
      };

      "dev" = {
        user = "root";
        compression = true;
        controlMaster = "no";
      };

      "ops" = {
        user = "root";
        compression = true;
        controlMaster = "no";
      };

      "prod" = {
        user = "root";
        compression = true;
        controlMaster = "no";
      };

      "oc-tunnel" = {
        hostname = "otter";
        user = "ben";
        localForwards = [{
          bind.port = 18789;
          host.address = "127.0.0.1";
          host.port = 18789;
        }];
        extraOptions = {
          "RequestTTY" = "no";
          "RemoteCommand" = "/run/current-system/profile/bin/sleep infinity";
        };
      };
    };
  };

  # services.ollama = {
  #   enable = true;
  # };

  xdg = {
    enable = true;
    cacheHome = "${config.home.homeDirectory}/.cache";
  };
}
