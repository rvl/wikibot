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

config.yml

## TODO

* [ ] better formatting of search results.
* [ ] **access control** -- i.e. the bot will only join certain channels
  and respond to certain people.
* [ ] add better elasticsearch index parameters and mappings.
* [ ] Automatic table of contents generation.



[Pandoc]: https://pandoc.org/
[Shake]: http://shakebuild.com/
[ElasticSearch]: https://www.elastic.co/guide/en/elasticsearch/reference/5.0/index.html
[Bloodhound]: http://hackage.haskell.org/package/bloodhound
[slack-api]: http://hackage.haskell.org/package/slack-api
