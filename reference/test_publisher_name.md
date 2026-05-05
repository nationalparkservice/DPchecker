# Tests EML metadata for the publisher name

Tests EML metadata for the publisher name

## Usage

``` r
test_publisher_name(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter is not
  provided, it defaults to calling `load_metadata` in the current
  project directory.

## Value

invisibly returns `metadata`

## Details

`test_publisher_name()` test for the presence of the data package
publisher name. It fails with an error if the publisher name is missing,
and fails with a warning if the publisher name is not "National Park
Service" (as this is expected to be rare). Passes test if publisher name
is "National Park Service"

## Examples

``` r
if (FALSE) { # \dontrun{
test_publisher_name()
} # }
```
