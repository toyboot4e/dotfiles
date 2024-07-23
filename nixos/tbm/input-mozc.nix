{
  config,
  pkgs,
  ...
}: {
  # TODO: trying `fcitx5` from `home-manager` (is `QT_PLUGIN_PATH` exported?)
  i18n.inputMethod = {
    enabled = "fcitx5";
    fcitx5.addons = with pkgs; [fcitx5-mozc fcitx5-gtk];
  };
}
