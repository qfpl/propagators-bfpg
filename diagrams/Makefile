all: simples adds badds celsius upper contradiction

.PHONY: all clean simples adds badds celsius upper contradiction

%.pdf : %.dot
	dot $< -Tpdf -o $@

simples: prop.pdf cell1.pdf cell2.pdf cell3.pdf always1.pdf always2.pdf always3.pdf always4.pdf

adds: add1.pdf add2.pdf add3.pdf add4.pdf

badds: badd1.pdf badd2.pdf badd3.pdf badd4.pdf

celsius: celsius1.pdf celsius2.pdf celsius3.pdf celsius4.pdf celsius5.pdf celsius6.pdf celsius7.pdf celsius8.pdf celsius9.pdf celsius10.pdf celsius11.pdf celsius12.pdf celsius13.pdf celsius14.pdf celsius15.pdf celsius16.pdf

upper: upper1.pdf upper2.pdf upper3.pdf

contradiction: contradiction1.pdf contradiction2.pdf contradiction3.pdf

%.dot: src/Main.hs
	nix-shell --command "cabal run"

clean:
	rm -rf *.pdf *.dot

