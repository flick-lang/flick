{ flick }:
flick.overrideAttrs (old: {
  pname = "flick-library-check";
  cargoBuildNoDefaultFeatures = true;
  cargoCheckNoDefaultFeatures = true;
  meta = removeAttrs old.meta [ "mainProgram" ];
})
