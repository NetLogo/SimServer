# --- !Ups

CREATE TABLE `type_bundles` (
  `name` varchar(25) NOT NULL,
  `js` longtext NOT NULL,
  PRIMARY KEY (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

# --- !Downs

DROP TABLE type_bundles;

