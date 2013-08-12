## dhtcrawler2

dhtcrawler is a DHT crawler written in erlang. It can join a DHT network and crawl many P2P torrents. The program save all torrent info into database and provide an http interface to search a torrent by a keyword.

![screenshot](https://raw.github.com/kevinlynx/dhtcrawler/master/screenshot.png)

dhtcrawler2 is an extended version to [dhtcrawler](https://github.com/kevinlynx/dhtcrawler). It has improved a lot on crawling speed, and much more stable. 

This git branch maintain pre-compiled erlang files to start dhtcrawler2 directly. So you don't need to compile it yourself, just download it and run it to collect torrents and search a torrent by a keyword. 

Enjoy it!

## Usage

* install Erlang R16B or newer
* download mongodb and start mongodb first

        mongod --dbpath your-database-path --setParameter textSearchEnabled=true

* start **crawler**, on Windows, just click `win_start_crawler.bat`
* start **hash_reader**, on Windows, just click `win_start_hash.bat`
* start **httpd**, on Windows, just click `win_start_http.bat`
* wait several minutes and checkout `localhost:8000`

You can also compile the source code and run it manually. The source code is in `src` branch of this repo.

Also you can check more technique information at my blog site (Chinese) [codemacro.com](http://codemacro.com)

## Source code

dhtcrawler is totally open source, and can be used in any purpose, but you should keep my name on, copyright by me please. You can checkout dhtcrawler2 source code in this git repo **src** branch.

## Config

Most config value is in `priv/dhtcrawler.config`, when you first run dhtcrawler, this file will be generated automatically. And the other config values are passed by arguments to erlang functions. In most case you don't need to change these config values, except these network addresses.

## Mongodb Replica set

It's not related about dhtcrawler, but only Mongodb, try figure it yourself.

## Another http front-end

Yes of course you can write another http front-end UI based on the torrent database, if you're interested in it I can help you about the database format.

## Sphinx

Yes, dhtcrawler2 support **sphinx** search. There's a tool named `sphinx-builder` load torrents from database and create sphinx index. `crawler-http` can also search text by sphinx. All you need is to config something.

## LICENSE

See LICENSE.txt
