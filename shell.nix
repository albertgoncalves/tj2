with import <nixpkgs> {};
mkShell.override { stdenv = llvmPackages_14.stdenv; } {
    buildInputs = [
        ghc
        hlint
        ormolu
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}
