{ pkgs ? import <nixpkgs> {}
, ... }:
let

  inherit (pkgs) stdenv scala sbt nodejs jekyll python27Packages;

in
stdenv.mkDerivation {
  name = "funops";
  buildInputs = [
    scala
    sbt
    nodejs
    jekyll
    python27Packages.pygments
  ];

  shellHook = ''
    ${sbt}/bin/sbt tut
  '';
}
