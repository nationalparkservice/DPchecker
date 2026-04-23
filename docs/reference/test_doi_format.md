# Check DOI formatting

`test_doi_format()` runs some basic formatting checks. If your DOI is
absent, the test will fail with an error. If the DOI is not exactly 37
characters AND does not contain "doi: https://doi.org/10.57830/" the
test will fail with an error. The test passes if the entry in the
alternateIdentifier field is exactly 37 characters long and contains
"doi: https://doi.org/10.57830/". Please note that this is a very
cursory formatting check; it does not check to make sure the DOI is
active (it probably should not be at this stage of data package
authoring). It does not check to make sure it is correct or that it
correctly corresponds to anything on DataStore or elsewhere within the
metadata.

## Usage

``` r
test_doi_format(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

Invisibly returns `metadata`.

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- test_doi_format(metadata)
} # }
```
