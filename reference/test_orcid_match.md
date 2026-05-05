# Tests whether metadata creator matches the ORCiD profile

`test_orcid_match()` will only evaluate Creators that are individuals
(not organizations). If an ORCiD has been supplied, the function will
attempt to access the indicated ORCiD profile and test whether the last
name indicated on the ORCiD profile matches the surName indicated in
Metadata. If all surNames match the ORCiD profiles, the test passes. If
any surName does not match the indicated ORCID profile, the test fails
with an error.

## Usage

``` r
test_orcid_match(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter is not
  provided, it defaults to calling `load_metadata` in the current
  project directory.

## Value

invisibly returns `metadata`

## Details

Potential reasons for failing this test include having entered the wrong
ORCiD into metadata, having improperly formatted the ORCiD in metadata
(it should be listed as https://orcid.org/xxxx-xxxx-xxxx-xxxx - see
[`test_orcid_format()`](test_orcid_format.md)), having set your ORCiD
profile to "private" (in which case the function can't access the name
associated with the profile) or differences between the ORCiD profile
name and the name in metadata (such as maiden vs. married name,
transposing given and surnames, or variation in surName spelling).

## Examples

``` r
if (FALSE) { # \dontrun{
test_orcid_match()
} # }
```
