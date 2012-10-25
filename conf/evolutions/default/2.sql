# --- !Ups

# Making timestamps into BigInts

ALTER TABLE user_work MODIFY timestamp BIGINT UNSIGNED NOT NULL;
ALTER TABLE user_work_comments MODIFY timestamp BIGINT UNSIGNED NOT NULL;

# --- !Downs

ALTER TABLE user_work MODIFY timestamp int(20) unsigned NOT NULL;
ALTER TABLE user_work_comments MODIFY timestamp int(20) unsigned NOT NULL;