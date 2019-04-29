RACKET=/usr/local/bin/racket
RACO=/usr/local/bin/raco
MAIN=main.rkt
NIGHTLY=nightly.rkt

all:
	${RACO} make ${MAIN}
	PLTSTDERR="error info@gtp-checkup" ${RACO} test ${MAIN}

nightly:
	-find . -name "compiled" -exec rm -r {} \;
	PLTSTDERR="error info@gtp-checkup" ${RACKET} ${NIGHTLY}
