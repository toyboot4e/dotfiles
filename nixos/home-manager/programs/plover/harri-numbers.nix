{
  pkgs,
  plover-flake,
  sources,
  ...
}:

let
  # source = sources.plover-harri-numbers;
  source = sources.forked-harri-numbers;
in
pkgs.python3Packages.buildPythonPackage {
  pname = source.pname;
  inherit (source) src;
  inherit (source) version;
  buildInputs = [ plover-flake.packages.${pkgs.system}.plover ];

  # Force setuptools directory structure. Or I could fork though.
  format = "setuptools";
  #  preBuild = ''
  #    mkdir harri_numbers
  #    : > harri_numbers/__init__.py
  #    cp Harri_numbers.py harri_numbers/
  #    cat > setup.py <<EOF
  #from setuptools import setup
  #setup()
  #EOF
  #    cat > setup.cfg <<EOF
  #[metadata]
  #name = harri_numbers
  #[options]
  #packages =
  #  harri_numbers
  #py_modules =
  #  Harri_numbers
  #[options.entry_points]
  #plover.dictionary =
  #  custom = harri_numbers.Harri_numbers
  #EOF
  #        '';
  pythonImportsCheck = [ "harri_numbers" ];
}
