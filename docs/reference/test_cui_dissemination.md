# Test for CUI dissemination code

`test_cui_dissemination()` examines EML metadata for the presence of a
Controlled Unclassified Information (CUI) dissemination code. The
function fails with an an error if the code does not exist or does not
match a list of valid codes. If the valid code is not "PUBLIC" the test
will produce a warning. A valid code results in a pass.

## Usage

``` r
test_cui_dissemination(metadata = load_metadata(directory))
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
test_cui_dissemination()
} # }
```
