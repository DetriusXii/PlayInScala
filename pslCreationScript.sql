CREATE SCHEMA jdip AUTHORIZATION detriusxii;

CREATE TABLE jdip.games (
	name VARCHAR(40) PRIMARY KEY
);

CREATE TABLE jdip.players (
	name VARCHAR(40) PRIMARY KEY
);

CREATE TABLE jdip.game_player_relation (
	game_name VARCHAR(40) REFERENCES jdip.games (name),
	player_name VARCHAR(40) REFERENCES jdip.players (name),
	PRIMARY KEY(game_name, player_name)
);
