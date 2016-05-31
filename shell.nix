{ pkgs ? import <nixpkgs> {}
, ... }:
let

  # tools
  inherit (pkgs) stdenv scala sbt nodejs ruby bundler python27Packages;

  # libraries/build dependencies
  inherit (pkgs) lib lzip libxml2 libxslt file libffi;

  nokogiriCfg = lib.concatStringsSep " " [
    "--with-xml2-config=${libxml2}/bin/xml2-config"
    "--with-xslt-config=${libxslt}/bin/xslt-config"
  ];
in
stdenv.mkDerivation {
  name = "funops";
  buildInputs = [
    scala
    sbt
    nodejs
    ruby
    bundler
    libffi
    lzip
    libxml2
    libxslt
    python27Packages.pygments
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${lzip}/lib"
    export C_INCLUDE_PATH="${lzip}/include"
    bundle config --local path .bundle
    bundle config --local build.nokogiri -- ${nokogiriCfg}
    bundle install
  '';
}
