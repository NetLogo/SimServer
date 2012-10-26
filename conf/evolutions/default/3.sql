# --- !Ups

ALTER TABLE user_work ADD `type` TINYTEXT NOT NULL AFTER `user_id`

# --- !Downs

ALTER TABLE user_work DROP `type`