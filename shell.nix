{ pkgs ? import <nixpkgs> {} }:
  let
    docsifyCli = pkgs.writeShellScriptBin "docsify-cli"
      ''
        npx run docsify-cli
      '';
  in
    pkgs.mkShell {
      packages = with pkgs; [
        docsifyCli
        nodejs
      ];

      shellHook = ''
        npm install docsify-cli
      '';
    }
