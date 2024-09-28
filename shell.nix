{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    name = "piggyback";

    nativeBuildInputs = with pkgs.buildPackages; [ 
      vscode
      emacs
      git
      opam
    ];

    shellHook = ''
      eval $(opam env)
    '';
}

