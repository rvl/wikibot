# WikiBot

*Your GitHub Wiki is now a Slack Bot*

This is currently at "proof of concept" stage.

## Why?

Because GitHub Wiki search is a little bit crap. If you have a private
wiki then it can't be indexed by Google.

## How it works

1. The wiki git repo is checked out.
2. Markdown files are converted to HTML with [Pandoc]() and [Shake]().
3. [ElasticSearch]() indexes the HTML files (via [Bloodhound]()).
4. A [slack-api]() bot connects to Slack and responds to search commands.

## Configuration

Put configuration in `config.yml`, see
[`config-example.yml`](./config-example.yml) for the possible options.

## TODO

* [x] better formatting of search results.
* [ ] respond to private messages
* [x] respond to slash commands
* [ ] **access control** -- i.e. the bot will only join certain channels
  and respond to certain people.
* [x] paging of search results
* [ ] update search index when wiki is changed.
* [ ] error handling in bot loop
* [ ] error handling when searching
* [x] add better elasticsearch index parameters and mappings.
* [x] Automatic table of contents generation per page.
* [ ] Generate table of contents for entire wiki.
* [x] add pandoc metadata to elasticsearch index
* [x] add document last modified info to elasticsearch index
* [ ] Make pull request with additions to [Bloodhound][] package
* [ ] Make pull request with additions to [slack-api][] package
* [ ] support multiple collections of documents.

[Pandoc]: https://pandoc.org/
[Shake]: http://shakebuild.com/
[ElasticSearch]: https://www.elastic.co/guide/en/elasticsearch/reference/5.0/index.html
[Bloodhound]: http://hackage.haskell.org/package/bloodhound
[slack-api]: http://hackage.haskell.org/package/slack-api
