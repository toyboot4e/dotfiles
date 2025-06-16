{
  buildPythonPackage,
  plover,
}:

let
  sources = pkgs.callPackage ./_sources/generated.nix { };

in
buildPythonPackage rec {
  pname = "plover-onyx-layout";
  version = "1.0.0";
  src = sources.plover-harri-numbers;
  propegatedBuildInputs = [ plover ];
}
