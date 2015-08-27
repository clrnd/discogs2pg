BEGIN;

ALTER TABLE ONLY artist ADD CONSTRAINT artist_pkey PRIMARY KEY (id);
ALTER TABLE ONLY label ADD CONSTRAINT label_pkey PRIMARY KEY (id);
ALTER TABLE ONLY master ADD CONSTRAINT master_pkey PRIMARY KEY (id);
ALTER TABLE ONLY master_artist ADD CONSTRAINT master_artist_pkey PRIMARY KEY (master_id, artist_id);
ALTER TABLE ONLY release ADD CONSTRAINT release_pkey PRIMARY KEY (id);
ALTER TABLE ONLY release_artist ADD CONSTRAINT release_artist_pkey PRIMARY KEY (release_id, artist_id);
ALTER TABLE ONLY release_company ADD CONSTRAINT release_company_pkey PRIMARY KEY (release_id, company_id, entity_type);
ALTER TABLE ONLY release_extraartist ADD CONSTRAINT release_extraartist_pkey PRIMARY KEY (release_id, artist_id, role);
ALTER TABLE ONLY release_format ADD CONSTRAINT release_format_pkey PRIMARY KEY (release_id, format_name);
--ALTER TABLE ONLY release_identifier ADD CONSTRAINT release_identifier_pkey PRIMARY KEY (release_id, ??);
ALTER TABLE ONLY release_label ADD CONSTRAINT release_label_pkey PRIMARY KEY (release_id, catno);
ALTER TABLE ONLY release_video ADD CONSTRAINT release_video_pkey PRIMARY KEY (release_id, src);
ALTER TABLE ONLY track ADD CONSTRAINT track_pkey PRIMARY KEY (release_id, idx);
ALTER TABLE ONLY track_artist ADD CONSTRAINT track_artist_pkey PRIMARY KEY (track_idx, release_id, artist_id);
ALTER TABLE ONLY track_extraartist ADD CONSTRAINT track_extraartist_pkey PRIMARY KEY (track_idx, release_id, artist_id);

--ALTER TABLE ONLY release_artist ADD CONSTRAINT FOREIGN KEY (release_id) REFERENCES release(id);
--ALTER TABLE ONLY release_company ADD CONSTRAINT FOREIGN KEY (release_id) REFERENCES release(id);
--ALTER TABLE ONLY release_extraartist ADD CONSTRAINT FOREIGN KEY (release_id) REFERENCES release(id);
--ALTER TABLE ONLY release_format ADD CONSTRAINT FOREIGN KEY (release_id) REFERENCES release(id);
--ALTER TABLE ONLY release_identifier ADD CONSTRAINT FOREIGN KEY (release_id) REFERENCES release(id);
--ALTER TABLE ONLY release_label ADD CONSTRAINT FOREIGN KEY (release_id) REFERENCES release(id);
--ALTER TABLE ONLY release_video ADD CONSTRAINT FOREIGN KEY (release_id) REFERENCES release(id);

CREATE INDEX release_title_idx ON release (title);
CREATE INDEX release_country_idx ON release (country);
CREATE INDEX release_artist_id_idx ON release_artist (artist_id);
CREATE INDEX release_artist_releaseid_idx ON release_artist (release_id);
CREATE INDEX release_extraartist_id_idx ON release_extraartist (artist_id);
--This one record prevents index being created as of no interest to us we delete it
--DELETE from release_extraartist where release_id=4620841 and role='(Other,"Stage Sound & Light Technicians")';
CREATE INDEX release_extraartist_releaseid_idx ON release_extraartist (release_id);
CREATE INDEX track_artist_id_idx ON track_artist (artist_id);
CREATE INDEX track_extraartist_id_idx ON track_extraartist (artist_id);
CREATE INDEX release_label_name_idx ON release_label (label);
CREATE INDEX release_label_catno_idx ON release_label (catno);
CREATE INDEX label_name_idx ON label (name);
CREATE INDEX artist_name_idx ON artist (name);

COMMIT;
