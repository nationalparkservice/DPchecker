# Load Data

`load_data()` inspects the working directory for data files. Loads all
existing data files into a tibble.

## Usage

``` r
load_data(directory = here::here())
```

## Arguments

- directory:

  the directory where the data file(s) are found (i.e. your data
  package). Defaults to the current working directory. On exit, returns
  to the current working directory.

## Value

a tibble of .csvs

## Details

loads all data files in a specified directory (default is the working
directory) into a tibble for later use in congruence checking. Returns
the user to the working directory upon exit. Currently only supports
.csv files.

## Examples

``` r
data_pkg_dir <- DPchecker_example("BICY_veg")
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
my_data <- load_data(data_pkg_dir)
```
