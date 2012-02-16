# Memory blues

I recently started playing around with a [Linode](http://www.linode.com)
instance. It's a really nice service and I wanted to kick the tires  with couple of
Haskell projects. The smallest instance comes with 512MB of memory, and this should be
reasonably enough for everybody. Except for `ghc` and `ld`, it seems :)

## The problem

I installed [Yesod](http://www.yesodweb.com/) web framework from scratch using cabal

    cabal install yesod

I quickly noticed that installing software with `cabal` caused a huge load spike
to host. I whipped out my trusty [htop](ihttp://htop.sourceforge.net/) and
noticed that process `ld` (the linker). was sucking up 500MB of memory and
compilation was (understandably) stuck since the host was swapping all the
time.

The relevant [ticket for GHC](http://hackage.haskell.org/trac/ghc/ticket/5240)
contained some information. I also came across [a brilliant
posting](http://labs.scrive.com/2011/08/running-ghc-on-low-memory-computers/)
outlining some optimizations for limited memory hosts.

## The fix

I tested giving the options to ghc, but unfortunately some packages flag out
refused to compile with those. At least for some packages the linker parameters
given with `-optl` were passed to `gcc`. After some twiddling I decided to give up.

Then I came across a [Stack Overflow
discussion](ttp://stackoverflow.com/questions/3476093/replacing-ld-with-gold-any-experience)
about [`gold`](http://en.wikipedia.org/wiki/Gold_%28linker%29) linker, which is
experimental optimized linker. There's a Ubuntu package for that,
`binutils-gold` which conveniently replaces the system `ld` with the with the experimental optimized linker.

So, it just installed it with command

    sudo apt-get install binutils-gold

and rerun the

    cabal install yesod

The compilation seemed to go through smoothly, and `ld` didn't cause any memory
spikes. As such, using the `binutils-gold` package seemed to solve this particular memory
blues.

## The disclaimer

The `gold` linker is experimental software, and is known not to compile some
software correctly (such kernel modules). As such, using it is a tradeoff. In
my case the standard linker just didn't work.  Fortunately, the experimental
linker can be quite easily removed by uninstalling the `binutils-gold` package -
system will revert back to stock ld.

Anyway, if you happen to run into this particular problem, try the
`binutils-gold`. Rumour is that the optimized `ld` might even help with
`boost`-heavy C++-projects.
