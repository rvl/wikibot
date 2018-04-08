{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.wikibot;
  cacheDir = "/var/cache/wikibot";
  buildDir = "${cacheDir}/build";
  runDir = "/run/wikibot";
  user = "wikibot";
  configFile = "${runDir}/config.yaml";
  configFileContents = {
    docs_dir = cfg.docsDir;
    build_dir = buildDir;
    wiki_url = cfg.wikiURL;
    elastic = {
      server = if cfg.elasticsearch.server != null
        then cfg.elasticsearch.server
        else "http://${config.services.elasticsearch.listenAddress}:${toString config.services.elasticsearch.port}";
      index_name = cfg.elasticsearch.indexName;
    };
    server = {
      listen_address = cfg.listenAddress;
      listen_port = cfg.port;
      base_url = "https://localhost/"; # fixme: remove
    };
    slack.respond = if cfg.slack.respond == [] then "*" else cfg.slack.respond;
  };
in {
  options.services.wikibot = {
    enable = mkEnableOption "Wikibot server";

    package = mkOption {
      type = types.package;
      example = literalExample "pkgs.haskellPackages.wikibot";
      default = pkgs.callPackage ../default.nix {};
      defaultText = "pkgs.callPackage ../default.nix {}";
      description = "Wikibot package to use";
    };

    docsDir = mkOption {
      description = ''
        Directory containing Wiki documents.
      '';
      type = types.path;
      example = "/srv/git/wiki";
    };

    wikiURL = mkOption {
      description = "URL of Wiki";
      type = types.str;
    };

    elasticsearch = {
      server = mkOption {
        description = ''
          Remote ElasticSearch 5 server URL. If null, then
          ElasticSearch will be configured to run on the local
          host.
        '';
        type = types.nullOr types.str;
        default = null;
      };

      indexName = mkOption {
        description = ''
          Index name for ElasticSearch.
        '';
        default = "wiki";
        type = types.str;
      };
    };
      
    listenAddress = mkOption {
      description = "Slack HTTP Events server listen address.";
      default = "127.0.0.1";
      type = types.str;
    };

    port = mkOption {
      description = "Slack Events server port to listen for HTTP traffic.";
      default = 4242;
      type = types.int;
    };

    slack = {
      apiTokenFile = mkOption {
        type = types.nullOr types.path;
        default = null;
        example = "/run/keys/slack-api-token";
        description = ''
          A file containing the Slack application full API token.
        '';
      };

      botTokenFile = mkOption {
        type = types.nullOr types.path;
        default = null;
        example = "/run/keys/slack-bot-token";
        description = ''
          A file containing the Slack application bot token.
        '';
      };

      verificationTokenFile = mkOption {
        type = types.nullOr types.path;
        default = null;
        example = "/run/keys/slack-verification-token";
        description = ''
          A file containing the Slack events API verification token.
        '';
      };

      respond = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = [ "@rvl" "#secret-channel" ];
        description = ''
          A list of channels and/or users that Wikibot will respond to.
          An empty list (the default) means the bot will respond to anyone.
        '';
      };
    };

    buildAfter = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "git-repo@wiki.service";
      description = ''
        Name of a systemd service which, when completed, will trigger
        rebuilding of the search index.
      '';
    };
  };

  config = mkIf cfg.enable {

    services.elasticsearch = mkIf (cfg.elasticsearch.server == null) {
      enable = mkDefault true;
      # Matches Database.V5.Bloodhound. Other versions aren't compatible.
      package = pkgs.elasticsearch5;
    };

    systemd.services.wikibot = {
      description = "Wikibot";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      requires = if cfg.elasticsearch.server == null then [ "elasticsearch.service" ] else [];

      preStart = ''
        set -e

        mkdir -p ${cacheDir} ${buildDir} ${runDir}
        chown -R wikibot:wikibot ${cacheDir} ${buildDir} ${runDir}
        chmod 770 ${cacheDir} ${buildDir} ${runDir}

        test_readable() {
          if [ ! -f $1 -o ! -r $1 ]; then
            echo "$1 is not a readable file" >&2
            exit 1
          fi
        }

        test_readable "${cfg.slack.apiTokenFile}"
        test_readable "${cfg.slack.botTokenFile}"
        test_readable "${cfg.slack.verificationTokenFile}"

        ( ${pkgs.jshon}/bin/jshon -e slack \
           -s "$(<'${cfg.slack.apiTokenFile}')" -i api_token \
           -s "$(<'${cfg.slack.botTokenFile}')" -i bot_token \
           -s "$(<'${cfg.slack.verificationTokenFile}')" -i verification_token \
           -p | ${pkgs.haskellPackages.json2yaml}/bin/json2yaml > ${configFile} ) <<'EOF'
        ${builtins.toJSON configFileContents}
        EOF

        chgrp wikibot ${configFile}
        chmod 640 ${configFile}
      '';

      serviceConfig = {
        ExecStart = "${cfg.package}/bin/wikibot -c ${configFile}";
        PermissionsStartOnly = true;
        User = user;
        Group = user;
      };
    };
    systemd.services.wikibot-build = {
      description = "Wikibot build and index";
      wantedBy = if cfg.buildAfter != null then [ cfg.buildAfter ] else [ ];
      after = if cfg.buildAfter != null then [ cfg.buildAfter ] else [ ];
      requires = if cfg.elasticsearch.server == null then [ "elasticsearch.service" ] else [ ];
      environment.WIKIBOT_CONFIG = configFile;
      path = with pkgs; [ git pandoc ];
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/build";
        User = user;
        Group = user;
      };
    };

    users.extraGroups.wikibot.gid = 216;
    users.extraUsers.wikibot = {
      group = "wikibot";
      uid = 216;
      description = "Wikibot user";
      home = "/homeless-shelter";
      createHome = false;
    };
  };
}
