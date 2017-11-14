all: simples adds mults badds celsius upper maybe perhaps doubleplus contradiction intervaladd tree lattices

.PHONY: all clean simples adds mults badds celsius upper maybe perhaps doubeplus contradiction intervaladd tree lattices

RUN_CABAL = nix-shell --command "cabal run"

HASKELL_SRCS = shell.nix src/Main.hs src/Dot.hs

shell.nix: propagator-talk-diagrams.cabal
	cabal2nix --shell . > shell.nix

%.dot : $(HASKELL_SRCS)
	$(RUN_CABAL)

%.pdf : %.dot
	dot $< -Tpdf -o $@

%.pdf : %.svg
	convert -background none $< $@

%.png : %.svg
	convert -background transparent $< $@

%.png: %.dot
	dot $< -Tpng -o $@

simples: prop.pdf cell1.pdf cell2.pdf cell3.pdf cell4.pdf always1.pdf always2.pdf always3.pdf always4.pdf

adds: add1.pdf add2.pdf add3.pdf add4.pdf

mults: mult1.pdf mult2.pdf mult3.pdf mult4.pdf

bimults: bimult1.pdf bimult2.pdf bimult3.pdf

badds: badd1.pdf badd2.pdf badd3.pdf badd4.pdf badd5.pdf badd6.pdf badd7.pdf badd8.pdf badd9.pdf badd10.pdf badd11.pdf badd12.pdf

celsius: celsius1.pdf celsius2.pdf celsius3.pdf celsius4.pdf celsius5.pdf celsius6.pdf celsius7.pdf celsius8.pdf celsius9.pdf celsius10.pdf celsius11.pdf celsius12.pdf celsius13.pdf celsius14.pdf celsius15.pdf celsius16.pdf

upper: upper1.pdf upper2.pdf upper3.pdf

maybe: maybe1.pdf maybe2.pdf maybe3.pdf maybe4.pdf

perhaps: perhaps1.pdf perhaps2.pdf

doubleplus: doubleplus0.pdf doubleplus1.pdf doubleplus2.pdf doubleplus3.pdf doubleplus4.pdf doubleplus5.pdf doubleplus6.pdf doubleplus7.pdf doubleplus8.pdf doubleplus9.pdf doubleplus10.pdf doubleplus11.pdf doubleplus12.pdf doubleplus13.pdf doubleplus14.pdf doubleplus15.pdf doubleplus16.pdf doubleplus17.pdf

contradiction: contradiction1.pdf contradiction2.pdf contradiction3.pdf contradiction4.pdf contradiction5.pdf contradiction6.pdf contradiction7.pdf contradiction8.pdf contradiction9.pdf

intervaladd: intervaladd1.pdf intervaladd2.pdf

tree: tree.pdf

lattices: powerset.pdf powerset-upside-down.pdf set/powerset-info1.pdf set/powerset-info2.pdf set/powerset-info3.pdf set/powerset-info4.pdf set/powerset1.pdf set/powerset2.pdf set/powerset3.pdf set/powerset4.pdf set/powerset5.pdf set/powerset6.pdf flat.pdf more-information.pdf dcpo.pdf

clean:
	rm -rf *.pdf *.dot *.pdf_tex *.svg set/*.pdf

