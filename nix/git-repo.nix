{ config, lib, pkgs, ... }:

with lib;

let
  repoOpts = {
    repo = mkOption {
      description = ''
        Git repository clone URL.
      '';
      type = types.str;
    };

    branch = mkOption {
      description = ''
        Branch to clone. If null then the repo default branch is used.
      '';
      type = types.nullOr types.str;
      default = null;
      example = "master";
    };

    path = mkOption {
      description = "Destination clone path.";
      type = types.path;
      # default = "/srv/git/" + name;
    };

    user = mkOption {
      description = ''
        User account under which to clone the repo.
      '';
      type = types.str;
      default = "nobody";
    };
    # fixme: umask?

    timerConfig = mkOption {
      description = ''
        Systemd timer config for polling of git repo. Same format as
        <option>systemd.user.timers.&lt;name&gt;.timerConfig</option>.
      '';
      type = types.attrsOf types.str;
      default = {
        OnUnitActiveSec = "10m";
        OnBootSec = "5m";
      };
    };

    netrc = mkOption {
      description = ''
        Path of a .netrc file containing usernames and passwords for
        accessing the repo. Permissions on this file can't be world
        readable.
      '';
      type = types.nullOr types.path;
      default = null;
    };
  };
in
{
  options.services.git-repos = mkOption {
    default = { };
    type = types.attrsOf (types.submodule { options = repoOpts; });
    description = ''
      Attribute set of git repos to update.
    '';
    example = literalExample
      ''
        { linux =
          { path = "/srv/git/linux";
            repo = "git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git";
          };
        }
      '';
  };


  config = let
    mkService = unit: listToAttrs (mapAttrsToList
      (name: cfg: nameValuePair "git-repo@${name}" (unit name cfg))
      config.services.git-repos);
  in {
    systemd.services = mkService (name: cfg:
      { description = "Update git repo ${cfg.path}";
        after = [ "network.target" ];
        path = [ pkgs.git ];
        serviceConfig.User = cfg.user;
        serviceConfig.PermissionsStartOnly = true;
        environment = if cfg.netrc != null then { CURLOPT_NETRC_FILE = cfg.netrc; } else { };
        preStart = ''
          set -e
          if [ ! -d "${cfg.path}" ]; then
            mkdir -p "${cfg.path}"
          fi
          chown ${cfg.user} "${cfg.path}"
        '';
        script = ''
          set -e

          if [ ! -d "${cfg.path}/.git" ]; then
            git -c "credential.helper=netrc -k -f ${cfg.netrc}" clone ${optionalString (cfg.branch != null) "--branch \"${cfg.branch}\""} "${cfg.repo}" "${cfg.path}"
          fi

          cd "${cfg.path}"
          git -c "credential.helper=netrc -k -f ${cfg.netrc}" fetch
          git reset --hard FETCH_HEAD
        '';
      });

    systemd.timers = mkService (name: cfg:
      { description = "Update git repo ${cfg.path}";
        partOf = [ "git-repo@${name}.service" ];
        wantedBy = [ "timers.target" ];
        inherit (cfg) timerConfig;
      });

  };
}
