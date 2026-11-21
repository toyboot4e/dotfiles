{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.tmux = {
    enable = true;
    sensibleOnTop = true;
    # Home Manager's base settings land after sensible and would undo it
    terminal = "tmux-256color";
    mouse = true;
    escapeTime = 20;
    historyLimit = 50000;
    focusEvents = true;
    aggressiveResize = true;
    plugins = with pkgs.tmuxPlugins; [
      fingers
      better-mouse-mode
      tmux-fzf
      resurrect
      # Must stay last: it appends its auto-save hook to status-right
      continuum
    ];
  };

  # Sourced before the plugins (default order 1000) so they see the @options and
  # continuum hooks the status-right set in view.conf
  xdg.configFile."tmux/tmux.conf".text = lib.mkOrder 750 ''
    source-file ${config.home.homeDirectory}/dotfiles/tool/tmux/tmux.conf
  '';

  # Stable path for the fzf script bindings in tmux.conf and bindings.conf
  xdg.configFile."tmux/plugins/tmux-fzf".source =
    "${pkgs.tmuxPlugins.tmux-fzf}/share/tmux-plugins/tmux-fzf";
}
