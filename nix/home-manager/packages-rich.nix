# Heavier toolchains and apps on top of `packages.nix`, for the full-featured machines
{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # dotfiles
    fastfetch
    xdg-ninja

    # SNS
    discord
    slack

    # Go
    go

    # Haskell
    ghc
    stack
    cabal-install
    haskell-language-server
    zlib
    ormolu
    haskellPackages.implicit-hie

    # JS
    nodejs

    # Python
    # FIXME: use pylsp installed with uv locally
    python3Packages.python-lsp-server
    uv

    # CI
    actionlint
    act
    circleci-cli

    # documentation
    pandoc
  ];
}
