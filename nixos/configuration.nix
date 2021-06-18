# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # automatic gc
  nix.gc = {
    automatic = true;
    dates = "03:15";
    options = "--delete-older-than 30d";
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      anonymousPro
      dejavu_fonts
      google-fonts
      inconsolata
      liberation_ttf
      powerline-fonts
      source-code-pro
      terminus_font
    ];
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the Plasma 5 Desktop Environment.
  #services.xserver.displayManager.sddm.enable = true;
  #services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.windowManager.i3.enable = false;
  services.xserver.windowManager.awesome = {
    enable = true;
    luaModules = [
      pkgs.luaPackages.vicious
    ];
  };

  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xlibs.xset}/bin/xset r rate 150 45
  '';

  systemd.user.services."xcape" = {
    enable = true;
    description = "xcape to use CTRL as ESC when pressed alone";
    wantedBy = [ "default.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.xcape}/bin/xcape";
  };

  systemd.user.services."autocutsel" = {
    enable = true;
    description = "AutoCutSel";
    wantedBy = [ "default.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStartPre = "${pkgs.autocutsel}/bin/autocutsel -fork";
    serviceConfig.ExecStart = "${pkgs.autocutsel}/bin/autocutsel -selection PRIMARY -fork";
  };

  services.syncthing = {
    enable = true;
    # user = "joedicastro";
    # dataDir = "/home/joedicastro/.config/syncthing";
  };
  # Configure keymap in X11
  # services.xserver.layout = "us";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;
  services.xserver.libinput.touchpad.tapping = false;
  # services.xserver.synaptics.enable = true;
  services.xserver.libinput.mouse.naturalScrolling = false;
  services.xserver.xkbOptions = "ctrl:nocaps";
  # sound.mediaKeys.enable = true;
  services.logind.lidSwitch = "lock";
  # services.logind.extraConfig = "HandleLidSwitch=ignore";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.benny = {
    isNormalUser = true;
    extraGroups = [ "wheel"  "networkmanager" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    starship
    rlwrap
    pandoc
    cask
    mu
    isync
    protonmail-bridge
    kubectl
    kubectx
    kubetail
    cloc
    metals
    fish
    tmux
    direnv
    pass
    fzf
    htop
    jq
    silver-searcher
    fd
    sbcl
    clojure
    leiningen
    syncthing
    tree
    python37
    python37Packages.pip
    python37Packages.python-language-server
    mosh
    rofi
    i3lock-fancy
    xsel
    mpd
    scrot
    unclutter
    alock
    parted
    acpi
    picom
    ripgrep
    nitrogen
    feh
    alsaUtils
    clipmenu
    arandr
    xfce.xfce4-screenshooter
    networkmanager_dmenu
    xorg.xbacklight
    pavucontrol
    xfce.xfce4-taskmanager
    ncmpcpp
    neovim
    ranger
    pcmanfm
    xcompmgr
    dmenu
    neofetch
    autocutsel
    xclip
    alacritty
    xcape
    wget
    vim
    firefox
    networkmanager
    emacs
    awesome
    fd
    qutebrowser
    git
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  programs.light.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  networking.networkmanager.enable = true;
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
