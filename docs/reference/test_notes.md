# Examines the additionalInfo elment of EML metadata

`test_notes()` extracts the additionalInfo components of EML metadata.
These elements will be used to populate the "Notes" section on the
DataStore landing page. If the Notes section is blank, the test fails
with a warning. If the notes section contains non-standard characters
(such as &#13;) or more than two consecutive spaces, the test fails with
a warning. Otherwise the test passes. For all warnings, the user is
advised to use
[`EMLeditor::set_additional_info()`](https://rdrr.io/pkg/EMLeditor/man/set_additional_info.html)
to fix the additionalInfo section.

## Usage

``` r
test_notes(metadata = load_metadata(directory))
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
test_notes()
} # }
```
