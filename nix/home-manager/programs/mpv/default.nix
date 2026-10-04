sources:
{ pkgs, ... }:
let
  inherit (pkgs.mpvScripts) buildLua;
  script =
    name: args:
    buildLua (
      {
        inherit (sources.${name}) pname src;
        version = sources.${name}.date;
      }
      // args
    );
in
{
  programs.mpv = {
    enable = true;
    scripts = [
      (script "mpv-file-browser" {
        scriptPath = ".";
        passthru.scriptName = "file-browser";
      })
      (script "mpv-bookmarker" { scriptPath = "bookmarker-menu.lua"; })
      (script "mpv-zenyd-scripts" { scriptPath = "delete_file.lua"; })
    ];
  };
}
