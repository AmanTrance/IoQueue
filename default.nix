{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ zlib libz glibc ];

  LD_LIBRARY_PATH = with pkgs; ''
    ${zlib.outPath}/lib:${libz.outPath}/lib:${glibc.outPath}/lib
  '';
}
