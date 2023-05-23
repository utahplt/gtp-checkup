RACKET=/usr/local/bin/racket
RACO=/usr/local/bin/raco
MAIN=main.rkt
NIGHTLY=nightly.rkt

all:
	${RACO} make ${MAIN}
	PLTSTDERR="error info@gtp-checkup" ${RACO} test ${MAIN}

nightly:
	-find . -type d -name "compiled" | xargs rm -rf
	PLTSTDERR="error info@gtp-checkup" ${RACKET} ${NIGHTLY}
