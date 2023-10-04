# DPchecker 0.3.1

* Added `test_keywords()` to the list of function in the DPchecker.Rmd file 
* Added `test_keywords()` to the list of functions run by `run_congruence_checks()`
* Added new function, `test_keywords()` to test for presence of keywords in metadata; something that is required for EML extraction on DataStore.
* Updated `test_valid_filenames()` to accept filenames with hyphens (in addition to filenames with alphanumerics and underscore). Filenames still must start with a letter.
* Added the `test_dates_parse()` function to the list of functions in the DPchecker.Rmd file
* Added the `test_dates_parse()` function to the list of functions that are run when the fucntion `run_congruence_checks()` is called.
* Added the function `test_dates_parse()` to test whether the date formats supplied in the metadata match the values supplied in the data files
* Added a `NEWS.md` file to track changes to the package.
