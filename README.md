Discogs to PostgreSQL
=====================

[![Build Status](https://travis-ci.org/alvare/discogs2pg.svg?branch=master)](https://travis-ci.org/alvare/discogs2pg)

Import [Discogs' data dumps](http://www.discogs.com/data/) into Postgres efficiently.

Uses [Hexpat](http://hackage.haskell.org/package/hexpat) for parsing XML
and Postgres' [COPY](http://www.postgresql.org/docs/9.4/static/sql-copy.html) to store the result.

Inspired by [discogs-xml2db](https://github.com/philipmat/discogs-xml2db).

## Why?

I wanted Discogs' data in a Postgres database and I had some issues with the existing solution:

1. it was slow, I let it run for 5 hours and it didn't finish
2. SAX events parsing for nested data is too difficult to maintain IMHO
3. using INSERT is not efficient, and I didn't want UNLOGGED tables
4. I wanted some data it didn't import and didn't want some it did
5. performance, I wanted it to be fast on commodity hardware

## Installation

The projects uses [stack](https://github.com/commercialhaskell/stack) so it should be as easy as:

```
stack install
```

and adding stack's binary path to your PATH (if you haven't already).

### Usage

Supposing we downloaded the 20150810 dump:

* discogs_20150810_artists.xml.gz
* discogs_20150810_labels.xml.gz
* discogs_20150810_masters.xml.gz
* discogs_20150810_releases.xml.gz

and we didn't decompress them, then we can run on the repo's directory:

```
$ createdb discogs
$ psql discogs < sql/tables.sql
$ discogs2pg -g -d 20150810 -c dbname=discogs
$ # wait an hour or two ...
$ psql discogs < sql/indexes.sql
```

### Options

discogs2pg can has two forms of operation, by file and by date.

You can import a single file with `discogs2pg -c <blah> some_releases.xml` or
import releases, artists, labels and master for a date DATE
in the current directory with `discogs2pg -c <blah> -d DATE`.

Also the `-g | --gzip` option lets you import the compressed files directly.

If you want it to be even faster, and your computer can handle it, you can pass an optional `--aggressive`
to make it open all file in parallel. Consider that since we are using COPY it will open a
connection per table: that's 15 connections at once.

## Contributing

I don't have a Windows machine to build and test the binary, so I'd love it if someone could get into that.

Also, I can think of two things:

* someone with experience in DBs could improve the data structure and indexes/primary keys
* someone with experience in structuring Haskell projects could help with the types/typeclasses and modules
* the code uses lazy lists (for real), I'd like to refactor it with conduit or something

I'd be glad to hear suggestions and take pull requests, of course.
