# Test for presence of a license name

`test_license()` examines the licenseName element of EML metadata. If
there is no license name, the test fails with ab error. If the license
name does not match a list of valid license names, the test fails. If
the metadata contain a valid license name, but the license name and CUI
dissemination code do not agree, the test fails with an error.
Otherwise, the test passes. Additionally, if the license name is not
"Public Domain" or "Creative Commons Zero v1.0 Universal", the function
will produce a warning that the data package is not public.

## Usage

``` r
test_license(metadata = load_metadata(directory))
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
test_license()
} # }
```
