RACKET=racket
RACO=raco
MAIN=main.rkt
BIN_DIR=$(shell dirname $(shell which racket))

all:
	${RACO} make ${MAIN}
	PLTSTDERR="error info@gtp-checkup" ${RACKET} ${MAIN} ${BIN_DIR}
