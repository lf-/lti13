{ sources, ghcVer, example }:
import sources.nixpkgs {
  overlays = [ (import ./overlay.nix { inherit ghcVer example sources; }) ];
  config = { allowBroken = true; };
}
