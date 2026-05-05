# File Name Match

`test_file_name_match()` checks to see whether all data files (.csv)
within a specified directory are listed under the objectName (child of
physical) element in an EML metadata file in the same directory, and
vice versa. Mismatches will result in the test failing with an error
message.

## Usage

``` r
test_file_name_match(
  directory = here::here(),
  metadata = load_metadata(directory)
)
```

## Arguments

- directory:

  the directory where the data file(s) are found (i.e. your data
  package). Defaults to the current working directory. On exit, returns
  to the current working directory.

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

Invisibly returns `metadata`.

## Details

If a directory other than the current working directory is specified,
`test.file_name_match()` returns to the current working directory on
exit. Note that the metadata file must follow NPS naming conventions,
specifically ending in \*\_metadata.xml. `test.file_name_match()`
assumes there are the same number of data files in the directory as
dataTables in the metadata file.

## Examples

``` r
dir <- DPchecker_example("BICY_veg")
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_file_name_match(dir)
#> ✔ All data files are listed in metadata and all metadata files names refer to
#>   data files.
```
