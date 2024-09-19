# DPchecker 0.3.5 (develoment version)
* Add `test_project()` function to test for DataStore projects.
* Add unit tests for all optional eml elements
* Update documentation to reflect new `test_project()` function.

# DPchecker 0.3.4

2024-07-29
* Update license to CC0
* Bug fix from `test_missing_data()`

2024-06-24
* Fixed bug in `test_missing_data()` where if the order of files listed in metadata did not match the order of files produced by `list.files()` the function would evaluate the wrong file and produce inadvertent and unhelpful errors.
2024-02-05
* Fix bug in `test_date_range()` that was adding UTC to temporalCoverage
* `test_missing_data()` now also handles the missing data codes "blank" and "empty".
* Update `test_missing_data()` to default to flag whole files, not each column that has undocumented missing data. This condenses the error output when running `run_congruence_checks()`. When trouble shooting and attempting to pinpoint data that lack missing values, `test_missing_data()` can be run with the parameter detail_level = "columns".
2024-01-26
* Bugfixes for `test_dates_parse()` and `test_date_range()`: now ignore files that have times but no dates or date times.
* Bugfixes for `test_valid_fieldnames()`, `test_valid_filenames()`, `test_numeric_fields()`, `test_dates_parse()`, and `test_date_range()` - all the same bug; must be something deep in a dependency chain changed.
* Bugfix attempt for `test_fields_match()` reportedly needs more testing
* Add function `test_missing_data()` which scans data for NAs not documented in metadata

# DPchecker 0.3.3

* Bug fixes for `test_date_range()` and `test_dates_parse()`.
* Adjusted `test_datatable_urls()` and `test_datatable_urls_doi()` so that they work properly when there are no data table urls present in metadata.
* Move convert_datetime_format to QCkit; add QCkit as re-export to DPchecker
* Updated tabular_data_congruence.R for speed and stability as per codefactor suggests

# DPchecker 0.3.2

* Adjusted `test_date_range()` so that it can handle data columns that contain both dates and times. Times are truncated to midnight such that if the data collected on the first day indicated is considered "in range" and data collected on the last day indicated is considered "in range".
* Added `test_public_points()` to the list of functions in the DPchecker.Rmd file.
* Added `test_public_points()` to the list of functions run by `run_congruence_checks()`.
* Added `test_public_points()` function to test whether metadata contains GPS coordinates if the package is not public.
* Added `test_keywords()` to the list of function in the DPchecker.Rmd file. 
* Added `test_keywords()` to the list of functions run by `run_congruence_checks()`.
* Added new function, `test_keywords()` to test for presence of keywords in metadata; something that is required for EML extraction on DataStore.
* Updated `test_valid_filenames()` to accept filenames with hyphens (in addition to filenames with alpha-numerics and underscore). Filenames still must start with a letter.
* Added the `test_dates_parse()` function to the list of functions in the DPchecker.Rmd file.
* Added the `test_dates_parse()` function to the list of functions that are run when the function `run_congruence_checks()` is called.
* Added the function `test_dates_parse()` to test whether the date formats supplied in the metadata match the values supplied in the data files.
* Added a `NEWS.md` file to track changes to the DPchecker R package.
