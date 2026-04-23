# Test for presence of Intellectual Rights

`test_int_rights()` tests for the presence of text within the
intellectualRights element in EML formatted metadata. If text if
present, the test passes. Otherwise, the test fails. `test_int_rights()`
makes no attempt to parse the text or test whether it properly coincides
with the CUI dissemination codes or licenseName in the metadata. This is
a simple presence/absence test.

## Usage

``` r
test_int_rights(metadata = load_metadata(directory))
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
test_int_rights()
} # }
```
