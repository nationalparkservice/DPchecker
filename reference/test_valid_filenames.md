# Test File Names for Invalid Characters

`test_valid_filenames()` checks for file names in the metadata that
contain invalid special characters. Only underscores, hyphens, and
alphanumeric characters are permitted, and names must begin with a
letter. Currently, invalid filenames will result in the test failing
with a warning, otherwise the test passes.

## Usage

``` r
test_valid_filenames(metadata = load_metadata(here::here()))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

Invisibly returns `metadata`.

## Details

You should run [`test_file_name_match()`](test_file_name_match.md)
before you run this function, since this function only checks the file
names in the metadata.

## Examples

``` r
meta <- load_metadata(DPchecker_example("BICY_veg"))
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_valid_filenames(meta)
#> ✔ File names begin with a letter and do not contain spaces or special
#>   characters.
```
