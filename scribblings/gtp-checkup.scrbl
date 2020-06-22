#lang scribble/manual

@require[
  gtp-checkup/data/parse
  gtp-checkup/scribblings/plot
  (for-label
    racket/base
    racket/contract
    gtp-checkup
    (only-in typed/racket require/typed))]

@(define bm tt)

@; -----------------------------------------------------------------------------
@title{GTP Checkup}

Source: @url{https://github.com/bennn/gtp-checkup}

Contains a few configurations of the @hyperlink["https://docs.racket-lang.org/gtp-benchmarks/index.html"]{GTP benchmark programs},
 code for continuously testing their performance as Racket changes,
 and data from past runs.


@section[#:tag "gtp-checkup:basic-usage"]{Basic Usage}

To run a performance test on your machine:

@itemlist[
@item{Clone the repo
  @itemlist[
    @item{
    Either via @tt{git clone},
     or better yet via @exec{raco pkg install --clone gtp-checkup}
  }]}
@item{Run @exec{make}}
@item{See results on @litchar{STDOUT}, with a summary of errors at the bottom.}
]

The Makefile compiles and runs all scripts that match the pattern @tt{benchmarks/*/main.rkt}.
Each compile job and each run job has a time limit.
Run @exec{racket main.rkt --help} for more information.

@section[#:tag "gtp-checkup:version-history"]{Version History}

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


@section{Checkup API}

@defmodule[gtp-checkup]{
  Main entry point, for running the benchmarks and for importing a new benchmark.
}

@defproc[(gtp-checkup [bin racket-bin-dir/c]
                      [#:iterations i (or/c #f exact-positive-integer?)]
                      [#:timeout time-limit (or/c #f (cons/c exact-positive-integer? exact-positive-integer?))])
         void?]{
  Run the benchmarks using the given executables.
  Post results to @racket[gtp-checkup-logger] at the @racket['info] level.

  The value of @racket[i] determines the number of times to run each benchmark
   configuration.

  The value of @racket[time-limit] determines how long to wait for each
   configuration to compile and run.
  If @racket[time-limit] is a @racket[cons]-pair, then its @racket[car] is the compile-time
   limit and its @racket[cdr] is the run-time limit.
}

@defproc[(import-benchmark [dir directory-exists?]) void?]{
  Extract code from the given directory and create a new folder
   in the @filepath{benchmarks/} directory of the @racketmodname[gtp-checkup]
   repo.
  The directory @racket[dir] must have a different name than any existing
   benchmark, and it must match the subdirectory structure of a
   @hyperlink["https://docs.racket-lang.org/gtp-benchmarks/index.html"]{GTP benchmark}.
}

@defthing[gtp-checkup-logger logger?]{
  Receives messages about running times and timeouts.
  To subscribe to this logger, set @litchar|{PLTSTDERR="error info@gtp-checkup"}|
}


@defproc[(racket-bin-dir/c [x any/c]) boolean?]{
  Predicate for a directory that exists and contains @litchar{racket} and
  @litchar{raco} executables.
}

@section{Checkup data for @tt{racket/racket}}

@(define (format-machine-spec dir)
   (list
     "Machine info: "
     (url (format "https://github.com/bennn/gtp-checkup/tree/master/data/~a/README.md" (directory->machine-name dir)))))

@(define (format-machine-dataset dir)
   (list
     "Source data: "
     (url (format "https://github.com/bennn/gtp-checkup/tree/master/data/~a/" (directory->machine-name dir)))))

@(define dir-pict#
   (parameterize ((*wide-plot-width* 800))
     (make-all-machine-data-pict*)))


The plots in this section show the performance of different snapshots of the
 Racket language.
A snapshot begins with one commit to the @hyperlink["https://github.com/racket/racket/commits/master"]{@tt{racket/racket}}
 repository and includes contemporaneous commits to other @hyperlink["https://github.com/racket"]{main distribution} repositories.

Quick guide to plots:

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
          (item
            (list
              (tt (directory->machine-name dir))
              (itemize
                (item (format-machine-spec dir))
                (item (format-machine-dataset dir)))))))

The points labeled @bm{typed-worst-case} are for a configuration where:
 (1) every module is typed and (2) every import is guarded with contracts
 via @racket[require/typed].

The benchmarks come from @hyperlink["https://github.com/bennn/gtp-benchmarks/releases/tag/v1.0"]{GTP benchmarks v1.0}.

@(let ((v0 (for/first ((v (in-hash-values dir-pict#))) v)))
   (for/list ((bm (in-list (sort (hash-keys v0) symbol<?))))
     (cons
       (subsection (format "~a" bm))
       (for/list ((pict# (in-hash-values dir-pict#)))
         (hash-ref pict# bm)))))
