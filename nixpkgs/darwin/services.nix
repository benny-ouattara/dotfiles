{ config, pkgs, ... }:

let
  home-dir = builtins.getEnv ("HOME");
  log-dir = home-dir + "/.logs";
in {
  services.skhd.enable = true;
  services.sketchybar.enable = true;
  services.skhd.skhdConfig =
    (builtins.readFile (pkgs.substituteAll { src = ../../skhd/skhdrc; }));

  launchd.user.agents = {
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
        EnvironmentVariables = { PATH = "${config.environment.systemPath}"; };
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
  };
}
