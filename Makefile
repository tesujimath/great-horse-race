# Standard defs:
GHC  = ghc
RM   = rm -f

EXES =  GreatHorseRace

.PHONY: all

all: GreatHorseRace

GreatHorseRace: GreatHorseRace.hs
	$(GHC) --make -O -package wx -o $@ -optl-mwindows $<

clean:
	$(RM) *.o *.hi *.exe $(EXES)
