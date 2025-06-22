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

        tex = pkgs.texlive.combine {
          inherit (pkgs.texlive) scheme-medium inconsolata;
        };

        bundle = pkgs.stdenv.mkDerivation {
          name = "chunknet-bundle";
          nativeBuildInputs = [ R_dev ];
          src = ./.;
          buildPhase = ''
            mkdir -p $out
            R CMD build .
            cp *.tar.gz $out/
          '';
          installPhase = "true";
        };
        check =
          pkgs.runCommand "check"
            {
              nativeBuildInputs = [
                R_dev
                tex
                pkgs.html-tidy
              ];
              buildInputs = [ bundle ];
            }
            ''
              mkdir -p $out
              R CMD check --as-cran ${bundle}/*.tar.gz
              check_success=$?
              cp -r *.Rcheck $out
              exit $?
            '';

      in
      {
        packages = {
          default = chunknet;
          bundle = bundle;
        };
        devShells.default = pkgs.mkShell {
          buildInputs = [
            R_dev
            radian_dev_exec
          ];
        };
        checks.default = check;
      }
    );
}
