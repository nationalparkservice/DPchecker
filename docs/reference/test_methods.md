# Examines the Methods section of EML

`test_methods()` first extracts the methods from an EML object. If
methods are not present, the test fails with an error. If the methods
are present, the tests asks 1) Is the section longer than 20 words? If
not, the test fails with a warning. 2) Does the methods section contain
unconventional characters such as &#13;? If so, the test fails with a
warning. 2) Does the methods section contain additional spaces (more
than two consecuitive spaces)? If so, the test fails with a warning. If
all of the tests pass, the test as whole passes. For any error or
warning, users are advised to use
[`EMLeditor::set_methods()`](https://rdrr.io/pkg/EMLeditor/man/set_methods.html)
to correct the problem.

## Usage

``` r
test_methods(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter is not
  provided, it defaults to calling `load_metadata` in the current
  project directory.

## Value

invisible(metadata)

## Examples

``` r
if (FALSE) { # \dontrun{
test_methods()
} # }
```
