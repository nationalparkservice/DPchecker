# DPchecker development version

* Bug fixes for `test_date_range()` and `test_dates_parse()`.
* Adjusted `test_datatable_urls()` and `test_datatable_urls_doi()` so that they work properly when there are no data table urls present in metadata.

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
