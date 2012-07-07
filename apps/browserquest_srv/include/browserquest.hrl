-define(PLAYER_BUCKET, <<"player">>).

-record(location, {
	  x :: integer(),
	  y :: integer()
	 }).

-record(player, {
	  last_checkpoint :: #location{},
	  name :: string(),
	  id :: integer(),
	  group :: integer()
	 }).
	  


-define(HELLO, 0).
-define(WELCOME, 1).
-define(SPAWN, 2).
-define(DESPAWN, 3).
-define(MOVE, 4).
-define(LOOTMOVE, 5).
-define(AGGRO, 6).
-define(ATTACK, 7).
-define(HIT, 8).
-define(HURT, 9).
-define(HEALTH, 10).
-define(CHAT, 11).
-define(LOOT, 12).
-define(EQUIP, 13).
-define(DROP, 14).
-define(TELEPORT, 15).
-define(DAMAGE, 16).
-define(POPULATION, 17).
-define(KILL, 18).
-define(LIST, 19).
-define(WHO, 20).
-define(ZONE, 21).
-define(DESTROY, 22).
-define(HP, 23).
-define(BLINK, 24).
-define(OPEN, 25).
-define(CHECK, 26).
