GHC = ghc
FLAGS = -Wall -Wextra -dynamic -no-keep-hi-files -no-keep-o-files -O2
FILES = src/mutt_alias.hs
OUT_EXE = bin/mutt_alias

build:
	mkdir -p bin
	$(GHC) $(FLAGS) $(INCLUDES) -o $(OUT_EXE) $(FILES) $(LIBS)

clean:
	rm -f $(OUT_EXE)
