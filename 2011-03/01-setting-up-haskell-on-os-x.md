# How to set up Haskell on Mac OS X

This guide assumes, that you have the brilliant **Homebrew** installed. If not, I recommend that you go to the [Homebrew homepage](http://mxcl.github.com/homebrew/) and install it first.

## Resetting Cabal and GHC package database

Sometimes you fiddle around your installation, and end up with messed up system that won't compile anything non-trivial. Cabal might have gotten into twist. In this case, the easiest solution sometimes is to do a reset and reinstall. It won't take much time, but allows you to have clean slate.

Magic incatations needed for cleanup are following:

~~~ {.bash}
    brew uninstall haskell-platform ghc  
    rm -rf ~/.ghc ~/.cabal
~~~

These commands uninstall `ghc` (the haskell compiler) and `haskell-platform` (the standard library) and remove local databases.

## Set up Haskell

Using `brew`, install `haskell-platform` with `ghc` (if brew formula for ghc is missing, it's likely that ghc has been rolled into haskell-platform).

~~~ {.bash}
    brew install ghc haskell-platform
~~~

After the haskell-platform has been installed, update cabal database with command

~~~ {.bash}
    cabal update
~~~

## Add cabal binaries to PATH

This step depends a lot on your default shell. For `bash`, open `~/.bashrc` to an editor of your choice, and add following line to the end:

~~~ {.bash}
    export PATH=$PATH:~/.cabal/bin
~~~

Next, take the new PATH to use with command

~~~ {.bash}
    source ~/.bashrc
~~~

## Final words

Now you should have a working Haskell installation. Please note that sometimes your cabal installation may go corrupt (unsatiable or conflicting dependences) - in this case, you may need to use the reset described in the (first section.

