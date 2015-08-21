SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

CREATE TABLE artist (
    id integer NOT NULL,
    name text NOT NULL,
    realname text,
    urls text[],
    namevariations text[],
    aliases text[],
    releases integer[],
    profile text,
    members text[],
    groups text[],
    data_quality text
);

CREATE TABLE label (
    id integer NOT NULL,
    name text NOT NULL,
    contactinfo text,
    profile text,
    parent_label text,
    sublabels text[],
    urls text[],
    data_quality text
);

CREATE TABLE release (
    id integer NOT NULL,
    status text,
    title text,
    country text,
    released text,
    notes text,
    genres text[],
    styles text[],
    master_id int,
    data_quality text
);

CREATE TABLE releases_artists (
    release_id integer,
    artist_id integer,
    anv text,
    join_relation text,
    role text
);

CREATE TABLE releases_extraartists (
    release_id integer,
    artist_id integer,
    anv text,
    join_relation text,
    role text
);

CREATE TABLE releases_formats (
    release_id integer,
    format_name text,
    format_text text,
    qty integer,
    descriptions text[]
);

CREATE TABLE releases_labels (
    release_id integer,
    label text,
    catno text
);

CREATE TABLE releases_identifiers (
    release_id integer,
    description text,
    type text,
    value text
);

CREATE TABLE releases_videos (
    release_id integer,
    duration text,
    src text,
    title text
);

CREATE TABLE releases_companies (
    release_id integer,
    company_id integer,
    entity_type integer,
    entity_type_name text,
    catno text
);

CREATE TABLE track (
    release_id integer,
    position text,
    title text,
    duration text
);

CREATE TABLE tracks_artists (
    release_id integer,
    artist_id integer,
    anv text,
    join_relation text,
    role text
);

CREATE TABLE tracks_extraartists (
    release_id integer,
    artist_id integer,
    anv text,
    join_relation text,
    role text
);

CREATE TABLE master (
    id integer NOT NULL,
    title text,
    main_release integer NOT NULL,
    year int,
    notes text,
    genres text,
    styles text,
    role text,
    data_quality text
 );

CREATE TABLE masters_artists (
    artist_name text,
    master_id integer
);

CREATE TABLE masters_artists_joins (
    artist1 text,
    artist2 text,
    join_relation text,
    master_id integer
);

CREATE TABLE masters_extraartists (
    master_id integer,
    artist_name text,
    roles text[]
);

CREATE TABLE masters_formats (
    master_id integer,
    format_name text,
    qty integer,
    descriptions text[]
);


CREATE TABLE masters_images (
    master_id integer,
    type text,
    height integer,
    width integer,
    image_uri text
);

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;
