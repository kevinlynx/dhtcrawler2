{application, kdht, [
  {description, "DHT library"},
  {vsn, git},
  {registered, []},
  {applications, [kernel, stdlib]},
  {modules, [bencode, bucket, dht_id, dht_net, dht_state, kdht_sup, msg, 
  	search, storage, timer_monitor, vlog]},
  {mod, {}}
]}.

