{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = (with pkgs; [
    zlib
    nixpkgs-fmt
  ]);
}
