sources:
{
  pkgs,
  inputs,
  ...
}:
let
  plover-flake = inputs.plover-flake;
  # TODO: Include Python dictionary data
  # harri-numbers = import ./harri-numbers.nix {
  #   inherit plover-flake pkgs sources;
  #   plover-flake-nixpkgs = inputs.plover-flake-nixpkgs;
  # };
in
{
  programs.plover = {
    enable = true;
    package = plover-flake.packages.${pkgs.system}.plover.withPlugins (ps: [
      ps.plover-auto-reconnect-machine
      ps.plover-lapwing-aio
      ps.plover-console-ui
      ps.plover-tapey-tape
      ps.plover-python-dictionary
      ps.plover-stenobee
      # harri-numbers

      # ps.alleycat-link
      # ps.plover2cat
      # ps.plover-1password
      # ps.plover-auto-identifier
      # ps.plover-auto-reconnect-machine
      # ps.plover-cards
      # ps.plover-casecat-dictionary
      # ps.plover-cat
      # ps.plover-clippy
      # ps.plover-clippy-2
      # ps.plover-clr-trans-state
      # ps.plover-combo
      # ps.plover-comment
      # ps.plover-console-ui
      # ps.plover-controller
      # ps.plover-current-time
      # ps.plover-cycle-translations
      # ps.plover-debugging-console
      # ps.plover-delay
      # ps.plover-dict-commands
      # ps.plover-dictionary-builder
      # ps.plover-dictionary-patch
      # ps.plover-digitalcat-dictionary
      # ps.plover-eclipse-dictionary
      # ps.plover-emoji
      # ps.plover-engine-server-2
      # ps.plover-excel-dictionary
      # ps.plover-fancytext
      # ps.plover-french-extended-stenotype
      # ps.plover-german-syllatype
      # ps.plover-grandjean
      # ps.plover-hjson-dictionary
      # ps.plover-italian-stentura
      # ps.plover-json-lazy
      # ps.plover-lapwing-aio
      # ps.plover-last-translation
      # ps.plover-listening-lookup
      # ps.plover-local-env-var
      # ps.plover-maajik
      # ps.plover-markdown-dictionary
      # ps.plover-melani
      # ps.plover-merge-words
      # ps.plover-michela
      # ps.plover-midi
      # ps.plover-midi4text
      # ps.plover-minimal-english-stenotype
      # ps.plover-mod-z
      # ps.plover-modal-dictionary
      # ps.plover-next-stroke
      # ps.plover-ninja
      # ps.plover-number-format
      # ps.plover-oft-eva
      # ps.plover-open-url
      # ps.plover-palantype
      # ps.plover-palantype-DE
      # ps.plover-phenrsteno
      # ps.plover-phoenix-stenotype
      # ps.plover-platform-specific-translation
      # ps.plover-plugins-manager
      # ps.plover-portuguese
      # ps.plover-python-dictionary
      # ps.plover-q-and-a
      # ps.plover-regenpfeifer
      # ps.plover-retro-case
      # ps.plover-retro-everything
      # ps.plover-retro-quotes
      # ps.plover-retro-stringop
      # ps.plover-retro-surround
      # ps.plover-retro-text-transform
      # ps.plover-retro-untranslator
      # ps.plover-roll-the-dice
      # ps.plover-rpn-calculator
      # ps.plover-run-applescript
      # ps.plover-run-shell
      # ps.plover-russian-trillo
      # ps.plover-search-translation
      # ps.plover-sound
      # ps.plover-spanish-mqd
      # ps.plover-spanish-system-eo-variant
      # ps.plover-start-words
      # ps.plover-startup-py
      # ps.plover-steno-engine-hooks-logger
      # ps.plover-stenobee
      # ps.plover-stenograph
      # ps.plover-stenohid-test
      # ps.plover-stenotype-extended
      # ps.plover-stitching
      # ps.plover-svg-layout-display
      # ps.plover_system_switcher
      # ps.plover-tapey-tape
      # ps.plover-textarea
      # ps.plover-trayicon
      # ps.plover-treal
      # ps.plover-uinput
      # ps.plover-unused-xtest-output
      # ps.plover-vcs-plugin
      # ps.plover-vipe
      # ps.plover-vlc-commands
      # ps.plover-websocket-server
      # ps.plover-windows-brightness
      # ps.plover-word-tray
      # ps.plover-wtype-output
      # ps.plover-wpm-meter
      # ps.plover-xtest-input
      # ps.plover-yaml-dictionary
      # ps.spectra-lexer

    ]);

    # plover.cfg
    settings = {
      "Machine Configuration" = {
        machine_type = "Gemini PR";
        auto_start = true;
      };

      "Plugins" = {
        enabled_extensions = [
          "plover_lapwing_aio"
          "plover_auto_reconnect_machine"
          "plover_console_ui"
        ];
        # enabled_extensions = [
        # "alleycat-link"
        # "plover2cat"
        # "plover-1password"
        # "plover-auto-identifier"
        # "plover-auto-reconnect-machine"
        # "plover-cards"
        # "plover-casecat-dictionary"
        # "plover-cat"
        # "plover-clippy"
        # "plover-clippy-2"
        # "plover-clr-trans-state"
        # "plover-combo"
        # "plover-comment"
        # "plover-console-ui"
        # "plover-controller"
        # "plover-current-time"
        # "plover-cycle-translations"
        # "plover-debugging-console"
        # "plover-delay"
        # "plover-dict-commands"
        # "plover-dictionary-builder"
        # "plover-dictionary-patch"
        # "plover-digitalcat-dictionary"
        # "plover-eclipse-dictionary"
        # "plover-emoji"
        # "plover-engine-server-2"
        # "plover-excel-dictionary"
        # "plover-fancytext"
        # "plover-french-extended-stenotype"
        # "plover-german-syllatype"
        # "plover-grandjean"
        # "plover-hjson-dictionary"
        # "plover-italian-stentura"
        # "plover-json-lazy"
        # "plover-lapwing-aio"
        # "plover-last-translation"
        # "plover-listening-lookup"
        # "plover-local-env-var"
        # "plover-maajik"
        # "plover-markdown-dictionary"
        # "plover-melani"
        # "plover-merge-words"
        # "plover-michela"
        # "plover-midi"
        # "plover-midi4text"
        # "plover-minimal-english-stenotype"
        # "plover-mod-z"
        # "plover-modal-dictionary"
        # "plover-next-stroke"
        # "plover-ninja"
        # "plover-number-format"
        # "plover-oft-eva"
        # "plover-open-url"
        # "plover-palantype"
        # "plover-palantype-DE"
        # "plover-phenrsteno"
        # "plover-phoenix-stenotype"
        # "plover-platform-specific-translation"
        # "plover-plugins-manager"
        # "plover-portuguese"
        # "plover-python-dictionary"
        # "plover-q-and-a"
        # "plover-regenpfeifer"
        # "plover-retro-case"
        # "plover-retro-everything"
        # "plover-retro-quotes"
        # "plover-retro-stringop"
        # "plover-retro-surround"
        # "plover-retro-text-transform"
        # "plover-retro-untranslator"
        # "plover-roll-the-dice"
        # "plover-rpn-calculator"
        # "plover-run-applescript"
        # "plover-run-shell"
        # "plover-russian-trillo"
        # "plover-search-translation"
        # "plover-sound"
        # "plover-spanish-mqd"
        # "plover-spanish-system-eo-variant"
        # "plover-start-words"
        # "plover-startup-py"
        # "plover-steno-engine-hooks-logger"
        # "plover-stenobee"
        # "plover-stenograph"
        # "plover-stenohid-test"
        # "plover-stenotype-extended"
        # "plover-stitching"
        # "plover-svg-layout-display"
        # "plover_system_switcher"
        # "plover-tapey-tape"
        # "plover-textarea"
        # "plover-trayicon"
        # "plover-treal"
        # "plover-uinput"
        # "plover-unused-xtest-output"
        # "plover-vcs-plugin"
        # "plover-vipe"
        # "plover-vlc-commands"
        # "plover-websocket-server"
        # "plover-windows-brightness"
        # "plover-word-tray"
        # "plover-wtype-output"
        # "plover-wpm-meter"
        # "plover-xtest-input"
        # "plover-yaml-dictionary"
        # "spectra-lexer"
        #         ];

      };

      "System" = {
        name = "Lapwing";
      };

      # "System: Lapwing" = {
      #   dictionaries = [
      #     {
      #       enabled = true;
      #       path = "Harri_numbers.py";
      #     }
      #   ];
      # };

      # "Output Configuration".undo_levels = 100;
    };

  };
}
