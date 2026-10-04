# `init.el`

My Emacs configuration based on [leaf.el](https://github.com/conao3/leaf.el) and [evil](https://github.com/emacs-evil/evil).

## Bootstrapping

Either `nix run` or Emacs command can be used to generate `init.el` for initial startup:

```sh
$ nix run .#tangle
```

```sh
$ emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "init.org")'
```
