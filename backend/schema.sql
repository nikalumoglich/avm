DROP TABLE IF EXISTS `users`;
CREATE TABLE `users` (
  `id` BIGINT NOT NULL AUTO_INCREMENT,
  `name` varchar(100) DEFAULT NULL,
  `email` varchar(100) DEFAULT NULL,
  `hashed_password` varchar(100) DEFAULT NULL,
  PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

DROP TABLE IF EXISTS `sessions`;
CREATE TABLE sessions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`user_id` BIGINT UNSIGNED NULL,
	`expiration` BIGINT UNSIGNED NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

DROP TABLE IF EXISTS `permissions`;
CREATE TABLE permissions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`permission` varchar(100) DEFAULT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

DROP TABLE IF EXISTS `users_permissions`;
CREATE TABLE users_permissions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`user_id` BIGINT UNSIGNED NULL,
  	`permission_id` BIGINT UNSIGNED NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

DROP TABLE IF EXISTS `products`;
CREATE TABLE products (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`name` varchar(100) DEFAULT NULL,
	`description` varchar(100) DEFAULT NULL,
	`price_formula` varchar(100) DEFAULT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

DROP TABLE IF EXISTS `dimensions`;
CREATE TABLE dimensions (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`product_id` BIGINT UNSIGNED NULL,
	`name` varchar(100) DEFAULT NULL,
	`symbol` varchar(32) DEFAULT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

DROP TABLE IF EXISTS `images`;
CREATE TABLE images (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`key` varchar(100) DEFAULT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

DROP TABLE IF EXISTS `products_images`;
CREATE TABLE products_images (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`product_id` BIGINT UNSIGNED NULL,
  	`image_id` BIGINT UNSIGNED NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

DROP TABLE IF EXISTS `dimensions_images`;
CREATE TABLE dimensions_images (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`dimension_id` BIGINT UNSIGNED NULL,
  	`image_id` BIGINT UNSIGNED NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

DROP TABLE IF EXISTS `orders`;
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

DROP TABLE IF EXISTS `orders_dimensions`;
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
	`key` varchar(100) DEFAULT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

DROP TABLE IF EXISTS `order_interactions_images`;
CREATE TABLE order_interactions_images (
	`id` BIGINT NOT NULL AUTO_INCREMENT,
	`order_interaction_id` BIGINT NOT NULL,
	`image_id` BIGINT NOT NULL,
	PRIMARY KEY (id)
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4;

DROP TABLE IF EXISTS `order_interactions_videos`;
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

INSERT INTO images (`key`) VALUES
	 ('image1.jpg'),
	 ('image2.jpg'),
	 ('image3.jpg'),
	 ('image4.jpg');

INSERT INTO products (name,description,price_formula) VALUES
	 ('sofa','descrição do sofá','(100 + {altura} * 3) + {largura} * {profundidade} + 100'),
	 ('cama','descrição da cama','1000');

INSERT INTO products_images (product_id,image_id) VALUES
	 (1,1),
	 (2,3),
	 (1,4);

INSERT INTO users_permissions (user_id, permission_id) VALUES (1, 1);