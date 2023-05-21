RACKET=/usr/local/bin/racket
RACO=/usr/local/bin/raco
MAIN=main.rkt
NIGHTLY=nightly.rkt

all:
	PLT_TR_NO_CONTRACT_OPTIMIZE=1 ${RACO} make ${MAIN}
	PLT_TR_NO_CONTRACT_OPTIMIZE=1 PLTSTDERR="error info@gtp-checkup" ${RACO} test ${MAIN}

nightly:
	-find . -type d -name "compiled" | xargs rm -rf
	PLTSTDERR="error info@gtp-checkup" ${RACKET} ${NIGHTLY}
