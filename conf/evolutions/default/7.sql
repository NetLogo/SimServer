# --- !Ups

ALTER TABLE user_work CHANGE COLUMN period_id period_id tinytext NOT NULL AFTER run_id;

# --- !Downs

ALTER TABLE user_work CHANGE COLUMN run_id run_id tinytext NOT NULL AFTER period_id;

