# --- !Ups

# Create primary tables

CREATE TABLE `user_work` (
  `id` int(20) unsigned NOT NULL AUTO_INCREMENT,
  `timestamp` int(20) unsigned NOT NULL,
  `period_id` tinytext NOT NULL,
  `run_id` tinytext NOT NULL,
  `user_id` tinytext NOT NULL,
  `data` longtext NOT NULL,
  `metadata` text NOT NULL,
  `description` text NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8;

CREATE TABLE `user_work_comments` (
  `id` int(20) unsigned NOT NULL AUTO_INCREMENT,
  `ref_id` int(20) unsigned NOT NULL,
  `timestamp` int(20) unsigned NOT NULL,
  `user_id` tinytext NOT NULL,
  `comment` longtext NOT NULL,
  PRIMARY KEY (`id`),
  KEY `Comments Ref ID` (`ref_id`),
  CONSTRAINT `Comments Ref ID` FOREIGN KEY (`ref_id`) REFERENCES `user_work` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=utf8;

CREATE TABLE `user_work_supplements` (
  `id` int(20) unsigned NOT NULL AUTO_INCREMENT,
  `ref_id` int(20) unsigned NOT NULL,
  `type` tinytext NOT NULL,
  `data` longtext NOT NULL,
  `metadata` text NOT NULL,
  PRIMARY KEY (`id`),
  KEY `Supplements Ref ID` (`ref_id`),
  CONSTRAINT `Supplements Ref ID` FOREIGN KEY (`ref_id`) REFERENCES `user_work` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

# --- !Downs

DROP TABLE user_work;
DROP TABLE user_work_comments;
DROP TABLE user_work_supplements;