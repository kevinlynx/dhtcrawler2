## dhtcrawler2

This git branch maintain pre-compiled erlang files to start dhtcrawler2 directly. 

## Usage

* install Erlang R16B or newer
* download mongodb and start mongodb first

        mongod --dbpath your-database-path --setParameter textSearchEnabled=true

* start **crawler**, on Windows, just click `win_start_crawler.bat`
* start **hash_reader**, on Windows, just click `win_start_hash.bat`
* start **httpd**, on Windows, just click `win_start_http.bat`
* wait several minutes and checkout `localhost:8000`



