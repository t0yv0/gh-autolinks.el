{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-24.11;
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: let

    version = self.rev or "dirty";

    overlay = final: prev: {
        gh-autolinks = final.callPackage ./package.nix {
            inherit version;
            epkgs = final.emacsPackagesFor final.emacs;
        };
    };

    # https://github.com/NixOS/nixpkgs/issues/395169
    emacs-overlay = final: prev: {
        emacs = prev.emacs.override { withNativeCompilation = false; };
    };

    out = system: let
        pkgs = import nixpkgs { inherit system; overlays = [emacs-overlay]; };
    in {
        packages.default = (self.overlays.default pkgs pkgs).gh-autolinks;
    };

    in flake-utils.lib.eachDefaultSystem out // {
        overlays.default = overlay;
    };
}
