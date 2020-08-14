{ sources, ghcVer }:
import sources.nixpkgs {
  overlays = [ (import ./overlay.nix { inherit ghcVer; }) ];
  config = { allowBroken = true; };
}
