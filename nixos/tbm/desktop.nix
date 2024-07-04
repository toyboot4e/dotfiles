{ config, pkgs, ... }:

{
  # TODO: change cursor
  # home.pointerCursor = 
  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.gnome.adwaita-icon-theme;
    # size = 24;
    size = 32;
  };

  xdg.mimeApps = {
    enable = true;

    associations.added = {
      "application/pdf" = ["org.gnome.Evince.desktop"];
    };

    defaultApplications = {
      "application/pdf" = ["org.gnome.Evince.desktop"];

      # FIXME: not working correctly
      "text/html" = ["org.firefox.firefox.desktop"];
      "x-scheme-handler/http" = "org.firefox.firefox.desktop";
      "x-scheme-handler/https" = "org.firefox.firefox.desktop";
      "x-scheme-handler/about" = "org.firefox.firefox.desktop";
      "x-scheme-handler/unknown" = "org.firefox.firefox.desktop";
    };
  };

  # Let `home-manager` overwrite `mimeapps.list` so that it doesn't fail:
  xdg.configFile."mimeapps.list".force = true;
}
