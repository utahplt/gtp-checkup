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
Each compile job and each run job has a time limit to keep things from taking too long.

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


@section{Checkup data for @tt{racket/racket}}

@(define (format-machine-spec dir)
   (list
     (exec "uname -a") ":"
     (nested #:style 'inset (tt (directory->machine-uname dir)))))

@(define dir-pict#
   (parameterize ((*wide-plot-width* 550))
     (make-all-machine-data-pict*)))


The plots in this section show the performance of different commits to the
 Racket language.

@itemlist[
  @item{
    @emph{x-axis} = time, commits occur from left to right
  }
  @item{
    @emph{y-axis} = runtime (seconds), lower is better.
  }
  @item{
    Each plot shows all commits for one benchmark,
     each commit is represented by one point.
  }
  @item{
    If one commit is much worse than the previous one,
     then the line is labeled with a short hash of the new/bad commit.
  }
]

Each subsection has data for one benchmark.
The data comes from the following machines:
@(apply itemize
        (for/list ((dir (in-hash-keys dir-pict#)))
          (item (list (tt (directory->machine-name dir)) " $ " (format-machine-spec dir)))))

@(let ()
   (define v0 (for/first ((v (in-hash-values dir-pict#))) v))
   (for/list ((bm (in-list (sort (hash-keys v0) symbol<?))))
     (cons
       (subsection (format "~a" bm))
       (for/list (((dir pict#) (in-hash dir-pict#)))
         (list (tt (directory->machine-name dir))
               (hash-ref pict# bm))))))
