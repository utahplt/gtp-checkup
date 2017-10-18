RACKET=racket
RACO=raco
MAIN=main.rkt
BIN_DIR=$(shell dirname $(shell which racket))

PLTSTDERR="error info@gtp-checkup"

all:
	${RACO} make ${MAIN}
	${RACKET} ${MAIN} ${BIN_DIR}
