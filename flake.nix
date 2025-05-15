{
  inputs = {
    utils.url = "github:numtide/flake-utils";
    orcv.url = "github:jcai849/orcv";
  };
  outputs =
    {
      nixpkgs,
      utils,
      orcv,
      ...
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        chunknet = pkgs.rPackages.buildRPackage {
          name = "chunknet";
          version = "1.1";
          src = ./.;
          propagatedBuildInputs = [
            orcv.packages.${system}.default
            pkgs.rPackages.uuid
          ];

        };

        prod_pkgs = [ chunknet ];
        dev_pkgs = prod_pkgs ++ [ pkgs.rPackages.languageserver ];

        R_dev = pkgs.rWrapper.override { packages = dev_pkgs; };
        radian_dev = pkgs.radianWrapper.override { packages = dev_pkgs; };
        radian_dev_exec = pkgs.writeShellApplication {
          name = "r";
          runtimeInputs = [ radian_dev ];
          text = "exec radian";
        };

      in
      {
        packages.default = chunknet;
        devShell = pkgs.mkShell {
          buildInputs = [
            R_dev
            radian_dev_exec
          ];
        };
      }
    );
}
