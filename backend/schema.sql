CREATE TABLE `users` (
  `id` BIGINT NOT NULL AUTO_INCREMENT,
  `name` varchar(100) DEFAULT NULL,
  `email` varchar(100) DEFAULT NULL,
  `hashed_password` varchar(100) DEFAULT NULL,
  PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE sessions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`user_id` BIGINT UNSIGNED NULL,
	`expiration` BIGINT UNSIGNED NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE permissions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`permission` varchar(100) DEFAULT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE users_permissions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`user_id` BIGINT UNSIGNED NULL,
  	`permission_id` BIGINT UNSIGNED NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE products (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`name` varchar(100) DEFAULT NULL,
	`description` varchar(100) DEFAULT NULL,
	`price_formula` varchar(100) DEFAULT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE dimensions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`product_id` BIGINT UNSIGNED NULL,
	`name` varchar(100) DEFAULT NULL,
	`symbol` varchar(32) DEFAULT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE images (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`url` varchar(100) DEFAULT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE products_images (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`product_id` BIGINT UNSIGNED NULL,
  	`image_id` BIGINT UNSIGNED NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE dimensions_images (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`dimension_id` BIGINT UNSIGNED NULL,
  	`image_id` BIGINT UNSIGNED NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE orders (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`user_id` BIGINT NOT NULL,
  	`product_id` BIGINT NOT NULL,
	`opening_date` DATETIME,
	`closing_date` DATETIME,
	`price` BIGINT,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE orders_dimensions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`order_id` BIGINT NOT NULL,
  	`dimension_id` BIGINT NOT NULL,
	`value` BIGINT,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE order_interactions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`order_id` BIGINT NOT NULL,
	`author_id` BIGINT NOT NULL,
	`text` TEXT,
	`created_at` DATETIME,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE videos (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`url` varchar(100) DEFAULT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE order_interactions_images (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`order_interaction_id` BIGINT NOT NULL,
	`image_id` BIGINT NOT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

CREATE TABLE order_interactions_videos (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`order_interaction_id` BIGINT NOT NULL,
	`video_id` BIGINT NOT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

# ---------------------------------

INSERT INTO permissions (permission) VALUES ('userLevel');
INSERT INTO permissions (permission) VALUES ('adminLevel');

INSERT INTO dimensions (product_id,name,symbol) VALUES
	 (1,'altura','{altura}'),
	 (1,'largura','{largura}'),
	 (1,'profundidade','{profundidade}');

INSERT INTO dimensions_images (dimension_id,image_id) VALUES
	 (1,2),
	 (2,3),
	 (3,4);

INSERT INTO images (url) VALUES
	 ('https://imgs.casasbahia.com.br/1535779538/1xg.jpg'),
	 ('https://imgs.casasbahia.com.br/1535779539/1xg.jpg'),
	 ('https://imgs.casasbahia.com.br/1535779540/1xg.jpg'),
	 ('https://imgs.casasbahia.com.br/1535779531/1xg.jpg');

INSERT INTO products (name,description,price_formula) VALUES
	 ('sofa','descrição do sofá','(100 + {altura} * 3) + {largura} * {profundidade} + 100'),
	 ('cama','descrição da cama','1000');

INSERT INTO products_images (product_id,image_id) VALUES
	 (1,1),
	 (2,3),
	 (1,4);

INSERT INTO users_permissions (user_id, permission_id) VALUES (1, 1);