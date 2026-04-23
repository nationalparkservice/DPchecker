# Generate path to example data

Generate path to example data

## Usage

``` r
DPchecker_example(dp_name = c("BICY_veg", "BUIS_herps"))
```

## Arguments

- dp_name:

  Name of data package.

## Value

Path to example data, if dp_name is specified.

## Examples

``` r
DPchecker_example()
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
#> [1] "C:/Users/rlbaker/Rdev_local/DPchecker/inst/extdata/BICY_veg"
DPchecker_example("BUIS_herps")
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
#> [1] "C:/Users/rlbaker/Rdev_local/DPchecker/inst/extdata/BUIS_herps"
```
