# Test metadata for attribute definitions

`test_attribute_defs()` extracts all attributeNames and
attributeDefinitions from EML metadata. It tests to make sure there are
the same number of attributeNames and attributeDefinitions. If true, the
test passes. If not, it fails with an error. This test ONLY looks at the
metadata, it does NOT look at the data files. Passing this test does not
mean that all data columns have attributes in the metadata associated
with them. To test that, see
[`test_fields_match()`](test_fields_match.md).

## Usage

``` r
test_attribute_defs(metadata = load_metadata(directory))
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
test_attribute_defs()
} # }
```
