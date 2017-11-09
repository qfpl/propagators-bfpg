all: simples adds mults badds celsius upper maybe doubleplus contradiction

.PHONY: all clean simples adds mults badds celsius upper maybe doubeplus contradiction

RUN_CABAL = nix-shell --command "cabal run"

HASKELL_SRCS = shell.nix src/Main.hs src/Diag.hs src/Dot.hs

shell.nix: propagator-talk-diagrams.cabal
	cabal2nix --shell . > shell.nix

%.pdf : %.dot
	dot $< -Tpdf -o $@

simples: prop.pdf cell1.pdf cell2.pdf cell3.pdf cell4.pdf always1.pdf always2.pdf always3.pdf always4.pdf

adds: add1.pdf add2.pdf add3.pdf add4.pdf

mults: mult1.pdf mult2.pdf mult3.pdf mult4.pdf

bimults: bimult1.pdf bimult2.pdf bimult3.pdf

badds: badd1.pdf badd2.pdf badd3.pdf badd4.pdf badd5.pdf badd6.pdf badd7.pdf badd8.pdf badd9.pdf badd10.pdf badd11.pdf

celsius: celsius1.pdf celsius2.pdf celsius3.pdf celsius4.pdf celsius5.pdf celsius6.pdf celsius7.pdf celsius8.pdf celsius9.pdf celsius10.pdf celsius11.pdf celsius12.pdf celsius13.pdf celsius14.pdf celsius15.pdf celsius16.pdf

upper: upper1.pdf upper2.pdf upper3.pdf

maybe: maybe1.pdf maybe2.pdf maybe3.pdf maybe4.pdf

doubleplus: doubleplus0.pdf doubleplus1.pdf doubleplus2.pdf doubleplus3.pdf doubleplus4.pdf doubleplus5.pdf doubleplus6.pdf doubleplus7.pdf doubleplus8.pdf doubleplus9.pdf doubleplus10.pdf doubleplus11.pdf doubleplus12.pdf doubleplus13.pdf doubleplus14.pdf doubleplus15.pdf doubleplus16.pdf doubleplus17.pdf

contradiction: contradiction1.pdf contradiction2.pdf contradiction3.pdf contradiction4.pdf contradiction5.pdf contradiction6.pdf contradiction7.pdf contradiction8.pdf contradiction9.pdf

%.dot: $(HASKELL_SRCS)
	$(RUN_CABAL)

clean:
	rm -rf *.pdf *.dot *.pdf_tex *.svg

