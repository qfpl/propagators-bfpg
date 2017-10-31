all: clean adds badds

.PHONY: all clean adds badds

%.pdf : %.dot
	dot $< -Tpdf -o $@

adds: add1.pdf add2.pdf add3.pdf add4.pdf

badds: badd1.pdf badd2.pdf badd3.pdf badd4.pdf

#add1.dot: dots
#add2.dot: dots
#add3.dot: dots
#add4.dot: dots

#badd1.dot: dots
#badd2.dot: dots
#badd3.dot: dots
#badd4.dot: dots

%.dot:
	nix-shell --command "cabal run"

clean:
	rm -rf *.pdf *.dot

