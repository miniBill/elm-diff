# elm-diff
A diff implementation for Elm.

This is a fork of [jinjor/elm-diff](https://github.com/jinjor/elm-diff), adding the possibility of considering two items similar.

## The algorithm

This library implements [Wu's O(NP) algorithm](http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf). It shares the idea with [Myers's O(ND) algorithm](http://www.xmailserver.org/diff2.pdf), but is much faster in some cases.

## LICENSE

BSD-3-Clause
