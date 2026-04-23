# Test for "by or for NPS"

Test for "by or for NPS"

## Usage

``` r
test_by_for_nps(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter is not
  provided, it defaults to calling `load_metadata` in the current
  project directory.

## Value

invisibly returns `metadata`

## Details

`test_by_for_nps()` test for presence of the "by or for NPS" field. The
test fails with an error if the information is missing from metadata.
The test fails with a warning if the metadata indicate that the data
were not created by or for the NPS (as this is expected to be relatively
rare). Otherwise the test passes.

## Examples

``` r
if (FALSE) { # \dontrun{
test_by_for_nps()
} # }
```
