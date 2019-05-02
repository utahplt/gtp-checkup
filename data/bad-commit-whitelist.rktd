;; bad-commit-whitelist
;; ---
;; This file contains a Racket list of commit hashes for commits to the
;; [racket/racket](https://github.com/racket/racket) repo.
;;
;; Each commit must have 2 properties:
;; 1. the `gtp-checkup` repo must have data that says the commit is
;;    _significantly worse_ than a previous commit
;; 2. the issue that caused the _worsening_ must have been fixed by a subsequent
;;    commit
;;
;; If a commit NOT in this list represents a problem,
;; then `raco setup gtp-checkup` reports the commit for a human to look at.
(
"006ec1bfc993c4b59657882c8a07aae3d896c81d"
"36c1f5724d04ed1b765cf7b612f44fe92b3961fd"
"4ed5d7d98b8f9f901eb055ef686f8e98a5814a6a"
"8d77b8403cbe9bdfa5133a71e10fd372895e52bd"
)
