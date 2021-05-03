# creahelpers
Helper functions used across crea packages

## GIS Directory
Some `creahelpers` functions need data stored in the so-called 'gis directory'. Content of this directory is available in a Google Bucket.

### Define path
`creahelpers` will look for gis directorypath in either:
- `gis_dir` variable in R global environment; or
- `GIS_DIR` variable in system environment. This can be set for instance using `GIS_DIR=` in `.Renviron' or '.env'.

### Sync content
To sync the content of your gis_directory with CREA Google Bucket, go to your gis directory and type:
`gsutil rsync -r . gs://crea-data/gis`
