# Browsers

qutebrowser is my choice.

## Firefox

- [Vimium-FF](https://addons.mozilla.org/ja/firefox/addon/vimium-ff/)
- [Tree Style Tab](https://addons.mozilla.org/ja/firefox/addon/tree-style-tab/) ([GitHub](https://github.com/piroor/treestyletab))
- In about:config, set `toolkit.legacyUserProfileCustomizations.stylesheets`: `true`
  - Linux: `~/.mozilla/firefox/*.default/chrome`
  - Mac: `~/Library/Application Support/Firefox/Profiles/*.default/chrome'`

## Vimuium settings

Mappings:

```
map ] nextTab
map [ previousTab
map { moveTabLeft
map } moveTabRight
```

Search options:

```
@w: https://www.wikipedia.org/w/index.php?title=Special:Search&search=%s Wikipedia

@b: https://bandcamp.com/search?item_type&q=%s Bandcamp
@y: https://www.youtube.com/results?search_query=%s Youtube

@doc: https://docs.rs/%s Docs.rs
@my: https://github.com/toyboot4e/%s My-repositories

@arch: https://wiki.archlinux.org/title/%s
@nix: https://search.nixos.org/packages?query=%s

@h: https://hoogle.haskell.org/?hoogle=%s
@hoogle: https://hoogle.haskell.org/?hoogle=%s
```

