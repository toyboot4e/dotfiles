{
  config,
  pkgs,
  ...
}:
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
      nerd-fonts.intone-mono
      # nerd-fonts.sauce-code-pro
      noto-fonts
      noto-fonts-cjk-sans
      # font-awesome
      # vistafonts roboto-mono
      # intel-one-mono
      # monoid pango
    ];

    fontconfig = {
      enable = true;
      defaultFonts = {
        # TODO: use SourceCodePro?
        serif = [ "noto-fonts-cjk-sans" ];
        sansSerif = [ "noto-fonts-cjk-sans" ];
        # "IntoneMono Nerd Font"
        monospace = [
          "intel-one-mono"
          "noto-sans-font-cjk"
        ];
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
