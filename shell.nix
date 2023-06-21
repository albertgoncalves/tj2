with import <nixpkgs> {};
mkShell.override { stdenv = llvmPackages_14.stdenv; } {
    buildInputs = [
        ghc
        hlint
        ormolu
        python3Packages.flake8
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}
