# xmonad

XMonad is a window manager written in Haskell for X11. It has a small core library and a lot of additional modules to extend the core functionality. Since XMonad can be used as a library to write the configuration as opposed to a lot of other window managers, which read from a configuration file, I have decided to write this small WM. It is essentially a large xmonad.hs file, but it features a customizable command line and a baked in panel.

## Installation

Installation requires `stack`. You can get it installed on your system through a utility called `ghcup`. After installing `stack` and cloning the repository, run the following command:

``` shell
$ cd xmonad
$ stack install
```

This will most likely use `$HOME/.local/bin` as the target directory. You may need to add it to your `$PATH` if you want to be able to run it directly.
