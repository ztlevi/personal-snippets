- Start ./bin/mongod -f ./conf/mongod.conf

- Connect mongo shell ./bin/mongo 127.0.0.1:12345/test

- Shutdown Server

  > use admin db.shutdownServer()

- Check log tail -f log/mongod.log

- DB manipulation

  > show dbs use XXX db.dropDatabase()

  > show collections db.XXXcollection.insert(XXXjson) for(i=3;i<100;i++) db.imooc_collection.insert({x:i})
  > db.imooc_collection.insert({x:1}) WriteResult({ "nInserted" : 1 }) db.XXXcollection.find()
  > db.imooc_collection.find({x:1}) { "\_id" : ObjectId("59f561e6bdd88fcf8ab7b650"), "x" : 1 }
  > db.imooc_collection.find() db.imooc_collection.find().count()
  > db.imooc_collection.find().skip(3).limit(2).sort({x:1}) { "\_id" : ObjectId("59f565bd9f57f37ba57397d0"), "x" : 6 } {
  > "\_id" : ObjectId("59f565bd9f57f37ba57397d1"), "x" : 7 } db.imooc_collection.find().skip(3).limit(2).sort({x:-1}) {
  > "\_id" : ObjectId("59f565bd9f57f37ba573982a"), "x" : 96 } { "\_id" : ObjectId("59f565bd9f57f37ba5739829"), "x" : 95
  > }
