{ config, pkgs, ... }:

let
  home-dir = builtins.getEnv("HOME");
  log-dir = home-dir + "/.logs";
in
{
  services.skhd.enable = true;
  services.skhd.skhdConfig = (builtins.readFile (pkgs.substituteAll {
    src = ../../skhd/skhdrc;
    tmuxEmulator = pkgs.writeShellScript "tmuxEmulator" ''
      open -a /Applications/kitty.app --args --config ${home-dir}/.config/kitty/kitty.conf -e ${pkgs.tmux}/bin/tmux
    '';
  }));

  launchd.user.agents =
    let iterate = name: interval: {
          StartInterval = interval;
          RunAtLoad = true;
          KeepAlive = true;
          StandardErrorPath = log-dir + "/" + name + ".log";
          StandardOutPath = log-dir + "/" + name + ".log";
          EnvironmentVariables = {
            PATH = "${config.environment.systemPath}";
          };
        };
    in {
      skhd = {
        serviceConfig = {
          RunAtLoad = true;
          StandardErrorPath = log-dir + "/" + "skhd" + ".log";
          StandardOutPath = log-dir + "/" + "skhd" + ".log";
        };
      };

      mcron = {
        serviceConfig = {
          RunAtLoad = true;
          StandardErrorPath = log-dir + "/" + "mcron" + ".log";
          StandardOutPath = log-dir + "/" + "mcron" + ".log";
          EnvironmentVariables = {
            PATH = "${config.environment.systemPath}";
          };
        };
        command = "/run/current-system/sw/bin/mcron --daemon";
      };

      # mbsync = {
      #   serviceConfig = {
      #     RunAtLoad = true;
      #     StandardErrorPath = log-dir + "/" + "mbsync" + ".log";
      #     StandardOutPath = log-dir + "/" + "mbsync" + ".log";
      #     EnvironmentVariables = {
      #       PATH = "${config.environment.systemPath}";
      #     };
      #   };
      #   command = "/run/current-system/sw/bin/mbsync gmail";
      # };

      # emacs = {
      #   serviceConfig = {
      #     RunAtLoad = true;
      #     StandardErrorPath = log-dir + "/" + "emacs" + ".log";
      #     StandardOutPath = log-dir + "/" + "emacs" + ".log";
      #   };
      #   command = "/usr/local/bin/emacs --daemon";
      # };

      # protonmail = {
      #   serviceConfig = {
      #     RunAtLoad = true;
      #     KeepAlive = true;
      #     StandardErrorPath = log-dir + "/" + "protonmail" + ".log";
      #     StandardOutPath = log-dir + "/" + "protonmail" + ".log";
      #   };
      #   command = "${pkgs.protonmail-bridge}/bin/protonmail-bridge";
      # };
    };
}
