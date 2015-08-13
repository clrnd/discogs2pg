Discogs to PostgreSQL
=====================

Import [Discogs data dumps](http://www.discogs.com/data/) into Postgre efficiently.

Uses [Hexpat](http://hackage.haskell.org/package/hexpat) for parsing XML
and Postgre's [COPY](http://www.postgresql.org/docs/9.4/static/sql-copy.html) to load it.
