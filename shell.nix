{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    name = "ocaml-dec";

    nativeBuildInputs = with pkgs.buildPackages; [ 
      emacs
      git
      opam
    ];

    shellHook = ''
      eval $(opam env)
    '';
}

