#lang scribble/manual

@(define bm tt)

@title{GTP Checkup}

Usage:

@itemlist[
@item{Install this package}
@item{Run @exec{make}}
]

Compiles and runs all @tt{main.rkt} scripts in the @filepath{benchmarks/} directory.
Each compile job and each run job has a time limit, to keep things from taking too long.

@itemlist[
  @item{
    @history[#:changed "0.1"
      @elem{Changed style of benchmarks to focus on one worst-case configuration
            instead of spot-checking N mixed ones.}]}
  @item{
    @history[#:changed "0.1"
      @elem{Renamed @bm{quadBG} to @bm{quadU} and replaced @bm{quadMB} with @bm{quadT}.
            The version notes for
            @other-doc['(lib "gtp-benchmarks/scribblings/gtp-checkup.scrbl")
                       #:indirect "GTP Benchmarks package"] explain why.
            Updated other benchmarks to match the GTP benchmarks versions.}]}
]

