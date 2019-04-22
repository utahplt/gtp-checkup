#lang scribble/manual

@require[
  gtp-checkup/data/parse
  gtp-checkup/scribblings/plot]

@(define bm tt)

@; -----------------------------------------------------------------------------
@title{GTP Checkup}

@; defmodulename ? ... anyway explain the motivation


@section{Usage}

@; TODO update the instructions ... maybe change the title here

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
            The version notes in
            @other-doc['(lib "gtp-benchmarks/scribblings/gtp-checkup.scrbl")
                       #:indirect "GTP Benchmarks"] explain why.
            Updated other benchmarks to match the GTP benchmarks versions.}]}
]


@section{Data}

@(define (format-machine-spec dir)
   (list
     (exec "uname -a") ":"
     (nested #:style 'inset (tt (directory->machine-uname dir)))))

@(for/list ((name+data (in-list (parameterize ((*wide-plot-width* 550))
                                  (make-all-machine-data-pict*)))))
   (list (section (directory->machine-name (car name+data)))
         (format-machine-spec (car name+data))
         (cdr name+data)))
