# Test creators for presence of an ORCiD

`test_orcid_exists()` will inspect the metadata and test each creator
listed as an individual person (individualName) but not creators that
are organizations for the presence of an ORCiD. If an ORCiD is found for
all individual creators, the test passes. If any individual creator
lacks an ORCiD, the test fails with a warning and the users is pointed
towards
[`EMLeditor::set_creator_orcids()`](https://rdrr.io/pkg/EMLeditor/man/set_creator_orcids.html)
to add ORCiDs if they so choose.

## Usage

``` r
test_orcid_exists(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter is not
  provided, it defaults to calling `load_metadata` in the current
  project directory.

## Value

invisibly returns `metadata`

## Examples

``` r
if (FALSE) { # \dontrun{
test_orcid_exists()
} # }
```
