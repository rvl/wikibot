# WikiBot

*Your GitHub Wiki is now a Slack Bot*

## Why?

Because GitHub Wiki search is a little bit crap. If you have a private
wiki then it can't be indexed by Google.

## How it works

1. The wiki git repo is checked out.
2. Markdown files are converted to HTML with [Pandoc][] and [Shake][].
3. [ElasticSearch][] indexes the HTML files (via [Bloodhound][]).
4. A [slack-api][] bot connects to Slack and responds to search commands.

## Configuration

Put configuration in `config.yml`, see
[`config-example.yml`](./config-example.yml) for the possible options.

## Building

This project depends on forked
[Bloodhound](https://github.com/rvl/bloodhound) and
[slack-api](https://github.com/rvl/slack-api) packages. Use the
provided nix files to build with the forked packages.

## TODO

* [x] better formatting of search results.
* [x] respond to private messages
* [x] respond to slash commands
* [x] only respond in channel when @addressed
* [x] **access control** -- i.e. the bot will only join certain channels
  and respond to certain people.
* [x] access control for /wikisearch slash command
* [x] paging of search results
* [x] button for less results
* [x] update search index when wiki is changed.
* [x] error handling in bot loop
* [x] error handling when searching
* [x] add better elasticsearch index parameters and mappings.
* [x] Automatic table of contents generation per page.
* [ ] Generate table of contents for entire wiki.
* [x] add pandoc metadata to elasticsearch index
* [x] add document last modified info to elasticsearch index
* [x] Make pull request with additions to [Bloodhound][] package
* [ ] Make pull request with additions to [slack-api][] package
* [ ] support multiple collections of documents.
* [x] NixOS module

[Pandoc]: https://pandoc.org/
[Shake]: http://shakebuild.com/
[ElasticSearch]: https://www.elastic.co/guide/en/elasticsearch/reference/5.0/index.html
[Bloodhound]: http://hackage.haskell.org/package/bloodhound
[slack-api]: http://hackage.haskell.org/package/slack-api
