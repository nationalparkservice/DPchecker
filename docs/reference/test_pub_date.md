# Test publication date presence and ISO-8601 formatting

`test_pub_date()` tests for presence and ISO-8601 formatting of the
publication date. Also tests whether the publication date is within a
reasonable bounds; i.e. not a date prior to the beginning of the data
package Reference Type (after 2022) and not in a future year. If the
publication date is missing or improperly formatted, the test fails with
an error. If the publication date is outside a reasonable range, the
test fails with a warning. Otherwiset the test passes.

## Usage

``` r
test_pub_date(metadata = load_metadata(directory))
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
meta <- load_metadata(DPchecker_example("BICY_Veg"))
test_pub_date(meta)
} # }
```
