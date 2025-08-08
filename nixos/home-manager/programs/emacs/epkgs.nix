# emacs packages
{ pkgs, sources }:
epkgs: [
  epkgs.treesit-grammars.with-all-grammars

  # meplaBuild:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/emacs/build-support/melpa.nix

  # Failed to build, due to the dependency of external command `git'.
  # (epkgs.melpaBuild {
  #   pname = "straight";
  #   version = "0.0.1";
  #   src = sources.emacs-straight.src;
  #   packageRequires = with epkgs; [ cl-lib ];
  #   ignoreCompilationError = false;
  # })

  # FIXME: This creates an error, but why??
  # (epkgs.melpaBuild {
  #   # pname = "smyx";
  #   pname = "smyx-theme";
  #   version = "0.0.1";
  #   src = sources.emacs-smyx.src;
  #   # packageRequires = with epkgs; [];
  #   # files = ["smyx-theme.el"];
  #   ignoreCompilationError = false;
  # })
]
