# --- !Ups

ALTER TABLE type_bundles CHANGE js action_js longtext NOT NULL;
ALTER TABLE type_bundles ADD presentation_js longtext NOT NULL;

# --- !Downs

ALTER TABLE type_bundles CHANGE action_js js longtext NOT NULL;
ALTER TABLE type_bundles DROP presentation_js;