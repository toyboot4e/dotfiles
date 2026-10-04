# Just a task runner
# <https://github.com/casey/just>

# shows this help message
help:
    @just -l

[private]
alias h := help

# formats the nix files
format:
    nix fmt flake.nix ssh-keys.nix _sources nix

[private]
alias fmt := format

# updates channels and flakes
update:
    sudo nix-channel --update
    nix flake update

[private]
alias u := update

# updates specific flake input
update-input name:
    nix flake update {{name}}

[private]
alias ui := update-input

fetch:
    nix run github:berberman/nvfetcher

[private]
alias f := fetch

switch:
    #!/usr/bin/env -S bash -euE
    host="$(whoami)"
    if [ $(uname) = Darwin ] ; then
        sudo nix run nix-darwin --extra-experimental-features 'flakes nix-command' -- switch --flake .#$host switch
    else
        sudo nixos-rebuild --flake .#$host switch
    fi

[private]
alias s := switch

boot:
    #!/usr/bin/env -S bash -euE
    host="$(whoami)"
    if [ $(uname) = Darwin ] ; then
        sudo nix run nix-darwin --extra-experimental-features 'flakes nix-command' -- switch --flake .#$host boot
    else
        sudo nixos-rebuild --flake .#$host boot
    fi

# activates home-manager only (symlinks, etc.) without the system switch
link:
    #!/usr/bin/env -S bash -euE
    host="$(whoami)"
    if [ $(uname) = Darwin ] ; then
        kind=darwinConfigurations
    else
        kind=nixosConfigurations
    fi
    config=".#$kind.$host.config.home-manager"
    out="$(nix build --no-link --print-out-paths "$config.users.$host.home.activationPackage")"
    # Normally exported by the nix-darwin/NixOS activation, which this bypasses
    HOME_MANAGER_BACKUP_EXT="$(nix eval --raw "$config.backupFileExtension")" "$out/activate"

[private]
alias l := link

# remove `plover.cfg` to avoid home-manager conflict
rm:
    #!/usr/bin/env -S bash -euE
    if [ $(uname) = Darwin ] ; then
        rm ~/Library/'Application Support'/plover/plover.cfg
    else
        rm ~/.config/plover/plover.cfg
    fi

vm:
    #!/usr/bin/env -S bash
    nixos-rebuild build-vm --flake .#"$(whoami)"
    just run-vm

run-vm:
    #!/usr/bin/env -S bash
    ./result/bin/run-"$(whoami)"-vm

gcroots:
    ls /nix/var/nix/gcroots/auto/

remove-gcroots:
    sudo rm /nix/var/nix/gcroots/auto/*
