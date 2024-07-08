{ config, pkgs, ... }:

{
  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  console = {
    # font = "Lat2-Terminus16";
    # keyMap = "us";
    # useXkbConfig = true; # use xkbOptions in tty.
    keyMap = "jp106";
    earlySetup = true;
    packages = with pkgs; [ terminus_font ];
    font = "ter-u14n";
  };

  # Fonts https://nixos.wiki/wiki/Fonts
  fonts = {
    fontDir.enable = true;
    enableDefaultPackages = true;

    # Font packages TODO: Add more fonts
    packages = with pkgs; [
      # SauceCodePro is distributed as SourceCodePro
      (nerdfonts.override { fonts = [ "IntelOneMono" "SourceCodePro" ]; })
      noto-fonts noto-fonts-cjk font-awesome pango monoid roboto-mono vistafonts
      intel-one-mono
    ];

    fontconfig = {
      enable = true;
      defaultFonts = {
        # TODO: use SourceCodePro?
        serif = [ "noto-fonts-cjk" ];
        sansSerif = [ "noto-fonts-cjk" ];
	# "IntoneMono Nerd Font"
        monospace = [ "intel-one-mono" "noto-sans-font-cjk" ];
      };

      # FIXME: seems to be not wokring
      antialias = true;
      hinting = {
        enable = true;
        style = "full"; # no difference?
        autohint = true; # no difference?
      };
    };
  };

  # Set your time zone.
  time.timeZone = "Asia/Tokyo";
}
