-define(PLAYER_BUCKET, <<"player">>).
-record(map, {json, attributes}).
-record(cp, {id, x, y, w, h}).
-record(mobarea, {id, x, y, w, h, type, nb}).
-record(player_state, {
	  id,
	  name,
	  armor,
	  weapon,
	  hitpoints,
	  pos_x,
	  pos_y,
	  checkpoint,
	  zone,
	  actionlist,
	  target,
	  local_cache
	 }).


% WS messages
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

% Game items
-define(WARRIOR, 1).

% Mobs
-define(RAT, 2).
-define(SKELETON, 3).
-define(GOBLIN, 4).
-define(OGRE, 5).
-define(SPECTRE, 6).
-define(CRAB, 7).
-define(BAT, 8).
-define(WIZARD, 9).
-define(EYE, 10).
-define(SNAKE, 11).
-define(SKELETON2, 12).
-define(BOSS, 13).
-define(DEATHKNIGHT, 14).

% Armors
-define(FIREFOX, 20).
-define(CLOTHARMOR, 21).
-define(LEATHERARMOR, 22).
-define(MAILARMOR, 23).
-define(PLATEARMOR, 24).
-define(REDARMOR, 25).
-define(GOLDENARMOR, 26).

% Objects
-define(FLASK, 35).
-define(BURGER, 36).
-define(CHEST, 37).
-define(FIREPOTION, 38).
-define(CAKE, 39).

% NPCs
-define(GUARD, 40).
-define(KING, 41).
-define(OCTOCAT, 42).
-define(VILLAGEGIRL, 43).
-define(VILLAGER, 44).
-define(PRIEST, 45).
-define(SCIENTIST, 46).
-define(AGENT, 47).
-define(RICK, 48).
-define(NYAN, 49).
-define(SORCERER, 50).
-define(BEACHNPC, 51).
-define(FORESTNPC, 52).
-define(DESERTNPC, 53).
-define(LAVANPC, 54).
-define(CODER, 55).

% Weapons
-define(SWORD1, 60).
-define(SWORD2, 61).
-define(REDSWORD, 62).
-define(GOLDENSWORD, 63).
-define(MORNINGSTAR, 64).
-define(AXE, 65).
-define(BLUESWORD, 66).


%% Orientations
-define(UP, 1).
-define(DOWN, 2).
-define(LEFT, 3).
-define(RIGHT, 4).
