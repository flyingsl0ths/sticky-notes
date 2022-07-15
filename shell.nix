{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = (with pkgs; [
    nodejs
    nodePackages.prettier
    nodePackages.sass

    cabal-install
    haskell-language-server
    haskellPackages.fourmolu
    haskellPackages.cabal-fmt
    (haskell.packages.ghc8107.ghcWithPackages (p: [ p.zlib ]))
    haskellPackages.hlint

    nixpkgs-fmt
  ]);
  # Optional
  shellHook =
    let
      # Primarily used for language servers/formatters when opening project in neovim/vim
      npmGlobalDirectory = ".npm-global";
    in
    ''
      export PATH="${builtins.getEnv "HOME"}/${npmGlobalDirectory}/bin:$PATH"
    '';
}
