# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:


{
  services.nixosManual.showManual = true;

  environment.systemPackages = with pkgs; [ 
    alsaLib alsaPlugins alsaUtils
    chromium flashplayer
    emacs
    feh
    file
    git
    gparted
    haskellPackages.haskellPlatform 
    haskellPackages.xmonad
    haskellPackages.xmonadContrib
    haskellPackages.ghcMod
    imagemagick
    swiProlog
    yap
    htop
    #mpc
    mpd
    mplayer
    darcs
    mupdf
    ncmpcpp
    ntfs3g
    psmisc
    transmission
    rxvt_unicode
    xautolock
    slock
    thunderbird
    weechat
    wget
    wine winetricks
    xbindkeys
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

  fonts.enableCoreFonts = true;
  fonts.enableGhostscriptFonts = true;
  fonts.extraFonts = with pkgs; [ vistafonts 
  		                  dejavu_fonts 
				  ubuntu_font_family
				  inconsolata
				  terminus_font
				  libertine
				  ttf_bitstream_vera
				];
  fonts.enableFontConfig = true;
  fonts.enableFontDir = true;
  
  users.extraUsers = {
    dries = { 
      createHome = true;
      home = "/home/dries";
      description = "dries";
      extraGroups = [ "wheel" ]; #"networkmanager" 
      useDefaultShell = true;
      group = "users";
      # openssh.authorizedKeys.keys = [ "ssh-dss AAAADHFLDHFLDdries@foobar_xsshpublickey" ];
    };
  };




  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
   


  # boot.initrd.kernelModules =
  #   [ # Specify all kernel modules that are necessary for mounting the root
  #     # filesystem.
  #     # "xfs" "ata_piix"
  #     # "acpi-cpufreq"
  #   ];

  boot.initrd.enableSplashScreen = false;

  boot.blacklistedKernelModules =
    [				
      "pcspkr"
      "acpi-cpufreq"
    ];
    
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.splashImage = null;

  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "desktop"; # Define your hostname.
  #networking.networkmanager.enable = true;
  #networking.networkmanager.packages = [ ];
  #networking.nameservers = [ "192.168.2.1" ];
  #networking.defaultGateway = "192.168.2.1";
  #networking.interfaces.eth0 = { ipAddress = "192.168.2.2"; prefixLength = 24; };
   
 # networking.wireless = {
 #    enable = true;  # Enables Wireless.
 #    driver = "ath9k";   
 #    interfaces = [ "wlan0" ];
 # };

  # hardware.enableAllFirmware = true;
  
  

  # Add filesystem entries for each partition that you want to see
  # mounted at boot time.  This should include at least the root
  # filesystem.
  fileSystems."/".device = "/dev/disk/by-label/nixos";
  fileSystems."/media/win" = { 
      device = "/dev/sda1";
      #fsType = "ntfs";
  };

  # boot.loader.grub.extraEntries = 
  #     "menuentry \"Windows Vista\" {
  #        title WindowsVista
  #        insmod ntfs
  #        set root='(hd0,0)'
  #        chainloader +1
  #      }";
   


  # List swap partitions activated at boot time.
  swapDevices =
  [ { device = "/dev/disk/by-label/swap"; } ];

  # Select internationalisation properties.
  i18n = {
      #consoleFont = "lat9w-16";
      consoleKeyMap = "be-latin1";
      defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Brussels";


  # List services that you want to enable:
  

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  


  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "be";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable the SLIM display manager
  services.xserver.autorun = true;
  services.xserver.displayManager.slim.defaultUser = "dries";
  services.xserver.displayManager.slim.autoLogin = true;
  
  # Enable the XMonad window manager
  #services.xserver.windowManager.xmonad.enable = true;
  #services.xserver.windowManager.default = "xmonad";
  #services.xserver.desktopManager.default = "none";
}
