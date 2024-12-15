{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    name = "ocaml-dev";

    nativeBuildInputs = with pkgs.buildPackages; [ 
      emacs
      git
      opam
    ];

    shellHook = ''
      eval $(opam env)
    '';
}

