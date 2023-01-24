# `configdata.yaml` contains default key mappings. As of today, it's at:
# https://github.com/qutebrowser/qutebrowser/blob/98fe159f99a7dae0c4b04969d1b0e03c9cef0a1c/qutebrowser/config/configdata.yml#L2803

def scaled(x):
    return 2 * x

def text_scaled(x):
    return 1 * x

FONT_SIZE=text_scaled(10)
DEFAULT_ZOOM=scaled(75)

config.load_autoconfig(False)

# Force dark mode. Required to restart qutebrowser
config.set('colors.webpage.darkmode.enabled', True)
# Don't flush on dark mode
config.set('colors.webpage.bg', 'black')

# Use a custom homepage
# required to use file:/// URI?
# c.url.default_page = '~/html/homepage.html'

# Constants
DL_VIDEO_DIR="~/Resources/videos"

# Tips:
# - `:config-source` to reload `config.py`
# - see `:help`, which contains cheatsheet

# Notes:
# - `cd` to clear downloads
# - name your tab with `set-mark`. It's binded to (TODO: what?)

# CAUTION: This hides the title bar, then you can't move or resize it anymore
#          (without setting `window.hide_decoration` in qutebrowser).
# Turn it off and place your qutebrowser first!
c.window.hide_decoration = True

# REMARK: I failed to have `password_fill` work

# --------------------------------------------------------------------------------
# System

c.content.pdfjs = True

config.set('auto_save.session', True)
c.auto_save.session = True

config.set('content.javascript.enabled', True, 'file://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')

# --------------------------------------------------------------------------------
# URLs

c.url.default_page = 'https://google.com/'
c.url.start_pages = 'https://qutebrowser.org/'

c.url.searchengines = {
    "DEFAULT": 'https://google.com/search?q={}',

    "b": 'https://bandcamp.com/search?q={}',
    "cr": 'https://crates.io/search?q={}',
    "d": 'https://dictionary.cambridge.org/dictionary/english/{}',
    'g': 'https://github.com/search?q={}',
    'h': 'https://hoogle.haskell.org/?hoogle={}',
    "s": 'https://stackoverflow.com/search?q={}',
    "y": 'https://www.youtube.com/results?search_query={}',
    "r": 'https://www.reddit.com/r/{}',

    "my": 'https://github.com/toyboot4e/{}',
    "doc": 'https://docs.rs/{}',
    "docs": 'https://docs.rs/{}',
    "lib": 'https://lib.rs/{}',

    "wiki-a": 'https://wiki.archlinux.org/title/{}',
    "wiki-n": 'https://nixos.wiki/index.php?search={}',
    # "nix": 'https://search.nixos.org/packages?query={}',
}

# --------------------------------------------------------------------------------
# Commands

# TODO: better pocket for support
c.aliases['pocket'] = 'open -t https://getpocket.com/edit?url={url}'

# Download video with 720p (FIXME)
c.aliases['dl'] = 'spawn youtube-dl {url} -o f"{DL_VIDEO_DIR}/%(title)s.%(ext)s"'

# --------------------------------------------------------------------------------
# Behaviors

# Open new tab next to the current tab
c.tabs.new_position.unrelated = "next"

# Open new tab with `F` without focusing it
config.set('tabs.background', True)

# ?
c.input.partial_timeout = 0

# Zoom
config.set('zoom.default', f"{DEFAULT_ZOOM}%")

# --------------------------------------------------------------------------------
# VIEW

c.tabs.position = "right"

c.tabs.width = scaled(160)

c.tabs.padding = {'bottom': 4, 'left': 4, 'right': 4, 'top': 4}

c.scrolling.bar = "always"
# c.content.user_stylesheets = "user.css"

# --------------------------------------------------------------------------------
# KEY BINDINGS

# disable `:tab-only` keybinding
config.unbind('co', mode = 'normal');

# Marks (m and M are for quickmarks by default)
# config.bind('m', 'set-cmd-text -s :set-mark ', mode='normal')
# config.bind('M', 'set-cmd-text -s :jump-mark ', mode='normal')

# TODO: mpv
config.bind('m', 'hint links spawn "~/.nix-profile/bin/mpv" {hint-url} --ontop --no-border', mode='normal')

# Quickmaarks
config.bind('sq', 'quickmark-save')

### EMACS-LIKE BINDINGS IN INSERT MODE ###
config.bind('<Ctrl+f>', 'fake-key <right>', mode='insert')
config.bind('<Ctrl+b>', 'fake-key <left>', mode='insert')

config.bind('<Ctrl+n>', 'fake-key <down>', mode='insert')
config.bind('<Ctrl+p>', 'fake-key <up>', mode='insert')

config.bind('<Ctrl+a>', 'fake-key <home>', mode='insert')
# NOTE: it goes to EoF..
config.bind('<Ctrl+e>', 'fake-key <end>', mode='insert')

config.bind('<Ctrl+d>', 'fake-key <delete>', mode='insert')
config.bind('<Ctrl+k>', 'fake-key <backspace>', mode='insert')

config.bind(
    '<Ctrl+l>', 'fake-key <shift-end> ;; fake-key <delete>', mode='insert')
config.bind(
    '<Ctrl+j>', 'fake-key <shift-home> ;; fake-key <delete>', mode='insert')

### Use `C-n` and `C-p` in the command line ###
config.bind("<Ctrl+n>", "completion-item-focus --history next", mode="command")
config.bind("<Ctrl+p>", "completion-item-focus --history prev", mode="command")

### ORDINARY-BROWSER-LIKE ###
config.bind('<Meta+f>', ':set-cmd-text /')

# history navigation Meta+[] ({} is not avaiable, why?)
config.bind('<Meta+[>', ':back')
config.bind('<Meta+]>', ':forward')
config.bind('<Ctrl+[>', ':back')
config.bind('<Ctrl+]>', ':forward')

# move focus (`[]`) and tabs (`{}`)
config.bind('[', ':tab-prev')
config.bind(']', ':tab-next')
config.bind('{', ':tab-move -')
config.bind('}', ':tab-move +')

# Pin
config.bind('<Meta+p>', ':tab-pin')

# Vim-like
config.bind('<Ctrl+e>', ':scroll down')
config.bind('<Ctrl+y>', ':scroll up')

# Vimium-like
config.unbind('d')  # use x, X
config.unbind('u')  # half page scroll
config.bind('x', ':tab-close')
config.bind('X', ':undo')

config.bind('yf', ':hint links yank')

config.bind('d', ':scroll-page 0 0.5')
config.bind('u', ':scroll-page 0 -0.5')

# --------------------------------------------------------------------------------
# COLOR

### LOAD COLOR THEME ###
# config.set('content.user_stylesheets', 'themes.solarized_dark.css')

# import themes.dracula.draw
# themes.dracula.draw.blood(c, {
#     'spacing': {
#             'vertical': 6,
#             'horizontal': 8
#         },
#     'font': {
#             'family': 'Menlo, Terminus, Monaco, Monospace',
#             'size': 10
#         }
# })

### COLOR ###
c.colors.completion.fg = '#d5c4a1'
c.colors.completion.odd.bg = '#333333'
c.colors.completion.even.bg = '#202020'
c.colors.completion.item.selected.bg = '#8fee96'
c.colors.completion.item.selected.border.top = '#151515'
c.colors.completion.item.selected.border.bottom = '#151515'
c.colors.completion.match.fg = '#d75f5f'

c.colors.downloads.bar.bg = '#202020'

c.colors.statusbar.normal.fg = '#d5c4a1'
c.colors.statusbar.normal.bg = '#202020'
c.colors.statusbar.command.fg = '#d4c5a1'
c.colors.statusbar.command.bg = '#202020'

c.colors.statusbar.url.fg = '#d5c4a1'
c.colors.statusbar.url.error.fg = '#d75f5f'
c.colors.statusbar.url.success.http.fg = '#84edb9'
c.colors.statusbar.url.success.https.fg = '#8fee96'
c.colors.statusbar.url.warn.fg = '#cd950c'

c.colors.tabs.bar.bg = '#202020'
c.colors.tabs.odd.fg = '#707070'
c.colors.tabs.odd.bg = '#202020'
c.colors.tabs.even.fg = '#707070'
c.colors.tabs.even.bg = '#202020'

c.colors.tabs.selected.odd.fg = '#d5c4a1'
c.colors.tabs.selected.odd.bg = '#202020'
c.colors.tabs.selected.even.fg = '#d5c4a1'
c.colors.tabs.selected.even.bg = '#202020'
c.fonts.tabs.selected = f"{FONT_SIZE}pt fantasque sans mono"
c.fonts.tabs.unselected = f"{FONT_SIZE}pt fantasque sans mono"

bg = '#051515'
c.colors.tabs.pinned.odd.fg = '#B9770E'
c.colors.tabs.pinned.odd.bg = bg
c.colors.tabs.pinned.even.fg = '#B9770E'
c.colors.tabs.pinned.even.bg = bg

# c.colors.tabs.pinned.selected.bg = '#D4AC0D'
# c.colors.tabs.pinned.selected.odd.fg = '#d5c4a1'
# c.colors.tabs.pinned.selected.odd.bg = '#202020'
# c.colors.tabs.pinned.selected.even.fg = '#d5c4a1'
# c.colors.tabs.pinned.selected.even.bg = '#202020'

