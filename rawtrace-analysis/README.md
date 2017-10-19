# Rawtrace Analyzer

The Rawtrace analyze is the tool used to convert the raw data extracted by mosgi into into usable information (i.e. extract SQL queries) by parsing the downloaded
Xdebug files. The resulting data is stored in a sqlite database. This holds both for the data extracted before the generation of the deep-model as well as
after running the csrf tests. The main files are wrapped in a bashscript to enable easier starting of the underlying lisp program.

## Rawtrace Acquisition


```
Usage: run-analyzer.sh [-m|--source-database-mosgi ARG]
                       [-v|--source-database-vilanoo ARG] [-d|--sink-database ARG] [-S|--sink-schema ARG]
                       [-f|--start-id ARG] [-e|--end-id ARG] [-k|--keep-all-queries ARG]

Available options:
  -m, --source-database-mosgi ARG
                           the database path of mosgis db from which to retrieve the information to analyze
  -v, --source-database-vilanoo ARG
                           the database path of vilanoos db from which to retrieve the information to analyze
  -d, --sink-database ARG  the database path into which to write the analyzed information
  -S, --sink-schema ARG    the schema for the sink database schema
  -f, --start-id ARG       the id from which to start the analysis of the http requests
  -e, --end-id ARG         the last id to process while analyzing the http requests
  -k, --keep-all-queries ARG
                           all SQL queries are kept and non state-changing ones are not removed (y/N)
```


## CSRF Test Analysis


```
run-analyzer.sh [-m|--source-database-mosgi ARG] [-d|--sink-database ARG]
                       [-S|--sink-schema ARG] [-f|--start-id ARG] [-e|--end-id ARG]
                       [-k|--keep-all-queries ARG]

Available options:
  -m, --source-database-mosgi ARG
                           the database path of mosgis db from which to retrieve the information to analyze
  -d, --sink-database ARG  the database path into which to write the analyzed information
  -S, --sink-schema ARG    the schema for the sink database schema
  -f, --start-id ARG       the id from which to start the analysis of the http requests
  -e, --end-id ARG         the last id to process while analyzing the http requests
  -k, --keep-all-queries ARG
                           all SQL queries are kept and non state-changing ones are not removed (y/N)
```
