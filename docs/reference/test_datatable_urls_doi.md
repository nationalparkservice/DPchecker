# Tests for data table URL formatting & correspondence with DOI

`test_datatable_urls_doi()` passes if all data tables have URLs that are
properly formatted (i.e.
"https://irma.nps.gov/DataStore/Reference/Profile/xxxxxxx") where
"xxxxxx" is identical to the DOI specified in the metadata. Fails with a
warning if there is no DOI specified in metadata. If a DOI is specified
in metadata, but the data table URL does not properly coincide with the
url for the landing page that the doi points to for any one table, the
test fails with a warning (and indicates which table failed). If data
table urls do not exist, fails with an error and indicates how to add
them.

## Usage

``` r
test_datatable_urls_doi(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

invisible(metadata)

## Examples

``` r
if (FALSE) { # \dontrun{
dir <- DPchecker_example("BICY_veg")
test_datatable_urls_doi(dir)
} # }
```
