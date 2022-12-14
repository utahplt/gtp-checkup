gtp-checkup
===
[![Build Status](https://travis-ci.org/bennn/gtp-checkup.svg)](https://travis-ci.org/bennn/gtp-checkup)

To test typed/untyped interaction:

```
$ racket main.rkt <BIN-DIR>
```

This compiles and runs a few small programs using the `raco` and `racket`
 executables from `<BIN-DIR>`.

There is also a Makefile. Running `make` is similar to `racket main.rkt $(which racket)`

```
$ make
```


Each compile task and run task has a time limit. Anything slower than 10min gets killed.
See `main.rkt` for details.

