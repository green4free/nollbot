CC = ghc
PROGRAM = nollbot

ALL: ${PROGRAM}

nollbot: nollbot.hs
	${CC} nollbot.hs

clean:
	rm -f ${PROGRAM} ${PROGRAM}.hi ${PROGRAM}.o


