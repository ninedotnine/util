GHC = ghc
FLAGS = -Wall -Wextra -dynamic -no-keep-hi-files -no-keep-o-files -O2
FILES = src/brighten.hs
OUT_EXE = bin/brighten

build:
	mkdir -p bin
	$(GHC) $(FLAGS) $(INCLUDES) -o $(OUT_EXE) $(FILES) $(LIBS)

clean:
	rm -f $(OUT_EXE)
