# Test Field Names for Invalid Characters

`test_valid_fieldnames()` checks for field names (e.g data column names)
in the metadata that contain invalid special characters. Only
underscores and alphanumeric characters are permitted, and names must
begin with a letter. If invalid column names exist, the test will fail
with a warning, otherwise the test passes.

## Usage

``` r
test_valid_fieldnames(metadata = load_metadata(here::here()))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

Invisibly returns `metadata`.

## Details

You should run [`test_fields_match()`](test_fields_match.md) before you
run this function, since this function only checks the field names in
the metadata.

## Examples

``` r
meta <- load_metadata(DPchecker_example("BICY_veg"))
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_valid_fieldnames(meta)
#> ✔ Field names begin with a letter and do not contain spaces or special
#>   characters.
```
