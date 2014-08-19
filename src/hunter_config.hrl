-record (player, {
    id,
    notifications = []
}).

-record (notification, {
    player_id,
    action,
    x,
    y
}).