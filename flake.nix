{
  description = "nix";
  inputs = {
    devx.url = "github:input-output-hk/devx";
    haskellNix.follows = "devx/haskellNix";
  };
  outputs = { self, flake-utils, devx, nixpkgs, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay ];
        };
        shell = devx.devShells.${system}.ghc962;
      in {
        devShells.default = with pkgs;
          mkShell {
            # FIXME: https://gitlab.haskell.org/ghc/ghc/-/issues/16590
            # NOTE: have to use the patched ghc from devx duue to the aboe issue.
            inherit (shell)
              buildInputs nativeBuildInputs propagatedBuildInputs
              propagatedNativeBuildInputs buildPhase phases preferLocalBuild;
            packages = [ freetype glew SDL2 ];
            shellHook = shell.shellHook + ''
              export DYLD_LIBRARY_PATH="${
                lib.makeLibraryPath [ SDL2 ]
              }:$DYLD_LIBRARY_PATH"
            '';
          };
      });
}
