# Test EML metadata for publisher city

`test_publisher_city()` inspects the publisher address in EML metadata.
It fails with an an error if the city element is empty. It fails with a
warning if it is not "Fort Collins". The test passes if it is "Fort
Collins"

## Usage

``` r
test_publisher_city(metadata = load_metadata(directory))
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
test_publisher_city()
} # }
```
