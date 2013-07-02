The scripts in this directory can help you to deply a repl set mongodb, which there is a primary database used by the crawler, and the secondary database is used to query by the http server.

[Check here if you know Chinese](http://www.cnblogs.com/dennisit/archive/2013/01/28/2880166.html)

Make sure mongod is in your path.

* db-start-primary.bat
* db-start-slave.bat
* init-primary-db.bat, make sure `rs.initiate()` success

