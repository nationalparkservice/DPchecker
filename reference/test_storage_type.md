# Tests for attribute storage type

`test_storage_type()` checks to see if there are the same number of
attributes (attributeName) and storageTypes in the metadata. Equal
numbers of elements will pass; unequal numbers will fail the test with
an error. `test_storage_type()` does NOT attempt to verify if the number
of storageType elements matches the number of columns in the data
package data files (for that functionality, use
[`test_fields_match()`](test_fields_match.md)).

`test_storage_type()` also verifies that the storageType is valid; i.e.
is a member of an accepted list of possible storage types. Currently
these are: string, float, date, factor, dateTime, or characters.
Validity for this test is based solely on observed ezEML/EAL output (in
theory any string in storageType is schema-valid). Invalid storageTypes
will result in a warning.

`test_storage_type()` does NOT attempt to verify that the value in
storageType logically matches the type data in the corresponding column.

## Usage

``` r
test_storage_type(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter is not
  provided, it defaults to calling `load_metadata` in the current
  project directory.

## Value

invisibly returns `metadata`

## Examples

``` r
if (FALSE) { # \dontrun{
test_storage_type()
} # }
```
