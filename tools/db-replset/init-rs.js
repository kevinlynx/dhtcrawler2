config_rs1 = {
    _id : "rs1",
    members : [ { _id:0, host:"localhost:27010", priority:1 }, { _id:1, host:"localhost:27017" } ]
};
rs.initiate(config_rs1);

