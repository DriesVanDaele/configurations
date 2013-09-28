# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:


{
  services.nixosManual.showManual = true;
  
  hardware.enableAllFirmware = true;

  environment.systemPackages = with pkgs; [
    #mpc
    acpi
    alsaLib alsaPlugins alsaUtils
    chromium flashplayer
    darcs
    emacs
    feh
    file
    git
    haskellPackages.ghcMod
    haskellPackages.haskellPlatform
    haskellPackages.xmonad
    haskellPackages.xmonadContrib
    htop
    imagemagick
    mpd
    mplayer
    mupdf
    mutt
    ncmpcpp
    nmap
    offlineimap
    psmisc
    rxvt_unicode
    sdcv
    stardict
    swiProlog
    transmission
    unrar
    unzip
    weechat
    wget
    xbindkeys
    yap
    (let myTexLive = 
      pkgs.texLiveAggregationFun {
        paths = [ texLive 
                  texLiveExtra 
                  texLiveBeamer 
                  #texLiveCMSuper 
                  #lmodern 
                  #texLiveContext
                ];
      };
    in myTexLive)
  ];

 # boot.extraKernelParams = [ "acpi_osi=Linux" "acpi_backlight=vendor" ];
  boot.blacklistedKernelModules = 
    [
      "pcspkr"
    ];

  fonts.enableGhostscriptFonts = true;
  fonts.extraFonts = with  pkgs; [ vistafonts
                                   dejavu_fonts
                                   ubuntu_font_family
                                   inconsolata
                                   terminus_font
                                   libertine
                                   ttf_bitstream_vera
                                 ];
  fonts.enableFontConfig = true;
  fonts.enableFontDir =  true;

  users.extraUsers = {
   dries = {
      createHome = true;
      home = "/home/dries";
      description = "dries";
      extraGroups = [ "wheel" ];
      useDefaultShell = true;
      group = "users";
    };
  };
	
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.initrd.kernelModules =
    [ # Specify all kernel modules that are necessary for mounting the root
      # filesystem.
      # "xfs" "ata_piix"
    ];
    
  #boot.kernelModules = [ "gma500_gfx" ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  networking = {
    hostName = "netbook"; # Define your hostname.
    interfaceMonitor.enable = false;
    wireless.enable = false;
    #useDHCP = false;
    wicd.enable = true;
  };

  fileSystems."/".device = "/dev/disk/by-label/nixos";

  # List swap partitions activated at boot time.
  swapDevices =
    [ { device = "/dev/disk/by-label/swap"; } ];

  # Select internationalisation properties.
  i18n = {
  #   consoleFont = "lat9w-16";
    consoleKeyMap = "be-latin1";
    defaultLocale = "en_US.UTF-8";
  };


  # boot.initrd.enableSplashScreen = false;
  boot.loader.grub.splashImage = null;

  time.timeZone = "Europe/Brussels";

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  
  # Enable the Dovecot 2.x POP3/IMAP server.
  # services.dovecot2.enable = true;
  # services.dovecot2.enableImap = true;
  # services.dovecot2.mailLocation = "maildir:~/Mail:INBOX=/var/spool/mail/%u";

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "be";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable the SLIM display manager	
  services.xserver.autorun = true;
  services.xserver.displayManager.slim.defaultUser = "dries";
  services.xserver.displayManager.slim.autoLogin = true;

  # services.xserver.windowManager.xmonad.enable = true;
  # services.xserver.windowManager.default = "xmonad";
  # services.xserver.desktopManager.default = "none";

  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.tapButtons = true;
  services.xserver.synaptics.twoFingerScroll = true;
}
