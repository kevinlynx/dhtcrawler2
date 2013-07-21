## 07.19.2013

* add simple json searhch api to http

## 07.15.2013

* crawler now will keep a hash cache, merge same hash in the cache, this makes hash_reader process less hashes

## 07.08.2013

* add torrent importer which can import local torrents into torrents database

## 07.05.2013

* add torrent downloader which will download torrents and store them in database or local file system
* hash_reader now use local torrents first, if not it will download, and depends on the config it may save the file too

