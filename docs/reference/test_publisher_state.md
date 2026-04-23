# Test EML metadata for publisher state

`test_publisher_state()` inspects the publisher address in EML metadata.
The test faisl with an error if the administrativeArea (state) element
is empty. The test fails with a warning if it is not "CO" (again,
because this is expected to be exceedingly rare). The test passes if it
is "CO".

## Usage

``` r
test_publisher_state(metadata = load_metadata(directory))
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
test_publisher_state()
} # }
```
