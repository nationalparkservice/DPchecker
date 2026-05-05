# Test Metadata for Duplicate Filenames

`test_dup_meta_entries()` tests to see whether there are duplicate
filenames listed for the data files in (EML) metadata.

## Usage

``` r
test_dup_meta_entries(metadata = load_metadata(here::here()))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

Invisibly returns `metadata`.

## Details

specifically, `test_dup_meta_entries()` looks at the 'physical' elements
of a metadata file, which describe each data file, and asks whether
there are duplicates entries under the objectName child element, which
is where the file name for each data file is stored. Duplicate entries
will result in the test failing with an error.

## Examples

``` r
meta <- load_metadata(DPchecker_example("BICY_veg"))
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_dup_meta_entries(meta)
#> ✔ Each data file name is used exactly once in the metadata file.
```
