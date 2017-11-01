all: simples adds badds celsius

.PHONY: all clean simples adds badds celsius

%.pdf : %.dot
	dot $< -Tpdf -o $@

simples: prop.pdf cell1.pdf cell2.pdf

adds: add1.pdf add2.pdf add3.pdf add4.pdf

badds: badd1.pdf badd2.pdf badd3.pdf badd4.pdf

celsius: celsius1.pdf celsius2.pdf celsius3.pdf celsius4.pdf celsius5.pdf

%.dot: src/Main.hs
	nix-shell --command "cabal run"

clean:
	rm -rf *.pdf *.dot

