{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, diagrams, diagrams-graphviz
      , diagrams-lib, diagrams-rasterific, diagrams-svg, fgl, graphviz
      , stdenv
      }:
      mkDerivation {
        pname = "propagator-talk-diagrams";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base containers diagrams diagrams-graphviz diagrams-lib
          diagrams-rasterific diagrams-svg fgl graphviz
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
