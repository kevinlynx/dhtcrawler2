-record(crawler_stats, {
	start_time = now(),
	announce_count = 0,
	get_peers_count = 0,
	torrent_count = 0, % valid torrent which has been downloaded even it's duplicated
	new_saved = 0, % saved as a new torrent into database
	updated = 0, % exist in database and update it
	torrent_sum % the sum torrent count
	}).

