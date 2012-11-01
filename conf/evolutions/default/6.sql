# --- !Ups

ALTER TABLE type_bundles ADD file_extension tinytext NOT NULL;

# --- !Downs

ALTER TABLE type_bundles DROP file_extension;