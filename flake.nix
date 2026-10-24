{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";

    microvm = {
      url = "github:microvm-nix/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      nixpkgs,
      microvm,
      self,
      ...
    }:
    {
      nixosConfigs = {
        test-vm-x86_64-linux = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            microvm.nixosModules.microvm
            {
              microvm = {
                vcpu = 4;
                shares = [
                  {
                    tag = "nix-store";
                    source = "/nix/store";
                    mountPoint = "/nix/.ro-store";
                    readOnly = true;
                  }
                  {
                    tag = "source";
                    source = "src";
                    mountPoint = "/home/webar/source";
                    readOnly = true;
                  }
                  {
                    tag = "config";
                    source = "config";
                    mountPoint = "/home/webar/config";
                    readOnly = true;
                  }
                  {
                    tag = "output";
                    source = "output";
                    mountPoint = "/home/webar/output";
                  }
                ];
                vsock.cid = 33625;
              };
            }
            ({ lib, pkgs, ... }: {
              environment.etc."webar/http-fetcher-proxy.json".text = builtins.toJSON {
                clients =
                  let
                    cfg = {
                      connector.http_tunnel = {
                        transport.vsock = {
                          cid = 2;
                          port = 10808;
                        };
                      };
                      req_per_sec = 32;
                    };
                  in
                  {
                    primary = cfg;
                    media_small = cfg;
                    media_large = cfg;
                  };
              };
              environment.systemPackages = with pkgs; [
                bindfs
              ];

              users.users.webar = {
                isNormalUser = true;
                password = "webar";
                extraGroups = [ "wheel" ];
              };
              security.sudo.wheelNeedsPassword = false;

              system.stateVersion = lib.trivial.release;
            })
          ];
        };
      };
      packages = {
        x86_64-linux = {
          test-vm = self.nixosConfigs.test-vm-x86_64-linux.config.microvm.declaredRunner;
        };
      };

      devShells = {
        x86_64-linux =
          let
            pkgs = nixpkgs.legacyPackages.x86_64-linux;
          in
          {
            default = pkgs.mkShell {
              packages = [
                pkgs.sqlite
                pkgs.slirp4netns
              ];
            };
          };
      };
    };
}
