# Test for the appropriate attribute for data table URLs in metadata

`test_datatable_url_attributes` tests whether the 'function =' attribute
for the element for each data table in metadata is properly specified.
If there is no attribute, the function is assumed to be a direct
download (as per the EML schema). The user is warned to check that this
is the case (as data packages on DataStore will typically have a direct
download link to the data file). If the specified attribute is not
either "information" or "download", the function will throw an error as
these are the only allowable attributes. If the attribute is "download"
the function will warn the user and ask them to double check this. If
the attribute is "information" and does not correspond to a DataStore
reference profile, the function will warn the user and ask them to check
this. If the attribute is "information" and a DataStore reference
profile page is supplied, the test will pass.

## Usage

``` r
test_datatable_url_attributes(metadata = load_metadata(directory))
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
test_datatable_url_attributes(metadata)
} # }
```
