# Test data and metadata data formats match

`test_dates_parse()` will examine all data columns that are described as
containing dates and times. Although it can handle multiple different
formats, the ISO-8601 format for dates and times is HIHGLY recommended
(ISO is YYYY-MM-DDThh:mm:ss or just YYYY-MM-DD). The function will
compare the format provided in the data files to the format indicated in
metadata. If there are no dates indicated in the metadata, the test
fails with a warning. If there are dates and the formats match, the test
passes. If the formats do not match, the test fails with an error. The
specific files and columns that failed are indicated in the results.

## Usage

``` r
test_dates_parse(directory = here::here(), metadata = load_metadata(directory))
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

Invisibly returns `metadata`

## Details

`test_dates_parse()` will examine EVERY cell in a column of dates until
it hits a date format that does not match the format specified in
metadata. For large datasets, this process can take a minute or two. If
there is even one typo in your data file, it will cause the function to
throw an error. Frequent source of error include viewing dates in Excel,
which can be deceptive, typos, and changes in the date format over time
or with changing personnel.

## Examples

``` r
dir <- DPchecker_example("BICY_veg")
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_dates_parse(dir)
#> ✔ Metadata and data date formatting is in congruence.
```
