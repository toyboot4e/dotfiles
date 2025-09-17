# fish

## fisher

I could set up homebrew, but it requires let Nix manage `config.fish`. Instead I'm installing plugins manually:

```sh
$ curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher
$ fisher install decors/fish-ghq
```

## Path

Add to PATH permanently:

```sh
$ fish_add_path -U ~/dev/bash/toy-scripts
```

