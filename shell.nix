with import <nixpkgs> {};

{ pkgs ? import <nixpkgs> {} }:
	pkgs.mkShell {
	  name = "droste-playground";
	  buildInputs = [
            sbt
          ];
}
