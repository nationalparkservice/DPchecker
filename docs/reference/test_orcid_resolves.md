# Test whether supplied Creator ORCiDs resolve to a valid ORCiD profile

`test_orcid_resolves()` will only examine ORCiDs that are supplied for
individual Creators (not organizations). If the ORCiD supplied consists
of a URL that leads to a valid ORCiD profile, the test passes. If the
ORCiD supplied is not a URL that resolves to a valid ORCiD profile -
either because the ORCiD itself does not exist or because the ORCiD was
supplied in an incorrect format, the test fails with an error. This test
does not examine Creators that do not have associated ORCiDs; if no
ORCiDs are provided that test does not return a pass or a fail.

## Usage

``` r
test_orcid_resolves(metadata = load_metadata(directory))
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
test_orcid_resolves()
} # }
```
