CREATE TABLE `users` (
  `id` BIGINT NOT NULL AUTO_INCREMENT,
  `name` varchar(100) DEFAULT NULL,
  `email` varchar(100) DEFAULT NULL,
  `hashed_password` varchar(100) DEFAULT NULL,
  PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

CREATE TABLE sessions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`user_id` BIGINT UNSIGNED NULL,
	`expiration` BIGINT UNSIGNED NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4
COLLATE=utf8mb4_0900_ai_ci;

CREATE TABLE permissions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`permission` varchar(100) DEFAULT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4
COLLATE=utf8mb4_0900_ai_ci;

INSERT INTO permissions (permission) VALUES ('userLevel');
INSERT INTO permissions (permission) VALUES ('adminLevel');

CREATE TABLE users_permissions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`user_id` BIGINT UNSIGNED NULL,
  `permission_id` BIGINT UNSIGNED NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4
COLLATE=utf8mb4_0900_ai_ci;