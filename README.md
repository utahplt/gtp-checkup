gtp-checkup
===
[![Build Status](https://travis-ci.org/bennn/gtp-checkup.svg)](https://travis-ci.org/bennn/gtp-checkup)

To test typed/untyped interaction:

```
$ racket main.rkt <BIN-DIR>
```

This compiles and runs a few small programs using the `raco` and `racket`
 executables from `<BIN-DIR>`.

There is also a Makefile. Running:

```
$ make
```

is similar to `racket main.rkt $(which racket)`


Should finish within 5 minutes.
