# fish

## fisher

I could set up homebrew, but it requires let Nix manage `config.fish`. Instead I'm installing plugins manually:

```sh
$ curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher
$ fisher install decors/fish-ghq # ctrl + g
$ fisher install PatrickF1/fzf.fish
$ fzf_configure_bindings --directory=\co # cmd + o
```

## Path

Add to PATH permanently:

```sh
$ fish_add_path -U ~/dev/bash/toy-scripts
```

