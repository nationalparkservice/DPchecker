# Load Metadata

`load_metadata()` loads the metadata file from a given path or
directory.

## Usage

``` r
load_metadata(directory = here::here(), inform_success = FALSE)
```

## Arguments

- directory:

  the directory where the metadata file is found - i.e. your data
  package. Defaults to your current project directory.

- inform_success:

  Boolean indicating whether to display a message when metadata is
  successfully loaded.

## Value

an R-object formatted as EML metadata.

## Details

Given a path or directory - default is the working directory -
`load_metadata()` looks for files with the ending \*\_metadata.xml. The
function quits with an error and tells the user if no such files are
found or if more than one such file is found. If only one metadata file
is found, it is checked for one of 3 formats: FGDC, ISO, or EML.
Currently only EML is supported and the function will fail with an
error, inform the user, and quit if non-EML metadata is found. The EML
metadata file is loaded into R's work space for future use during
congruence checking.

In the context of National Park Service data packages, this function can
be slightly easier to use for loading metadata into R than
[`EML::read_eml()`](https://rdrr.io/pkg/EML/man/read_eml.html) because
it does require a filename or type be specified.

## Examples

``` r
data_pkg_dir <- DPchecker_example("BICY_veg")
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
my_metadata <- load_metadata(data_pkg_dir)
```
