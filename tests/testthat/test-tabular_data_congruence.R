good_dir <- here::here("tests", "testthat", "good")
bad_dir <- here::here("tests", "testthat", "bad")

# ---- load_metadata ----

test_that("load_metadata works on valid EML file", {
  expect_message(load_metadata(here::here(good_dir, "BICY_good")),
                 ".*Metadata check passed.*")
  expect_message(load_metadata(here::here(good_dir, "BUIS_good")),
                 ".*Metadata check passed.*")
})

test_that("load_metadata throws an error for non-EML formats", {
  expect_error(load_metadata(here::here(bad_dir, "wrong_meta_formats", "fgdc")),
                 "Congruence checking is not yet supported for fgdc")
  expect_error(load_metadata(here::here(bad_dir, "wrong_meta_formats", "iso")),
                 "Congruence checking is not yet supported for ISO19915")
})

test_that("load_metadata throws an error for non-EML formats", {
  expect_error(load_metadata(here::here(bad_dir, "wrong_meta_formats", "fgdc")),
               "Congruence checking is not yet supported for fgdc")
  expect_error(load_metadata(here::here(bad_dir, "wrong_meta_formats", "iso")),
               "Congruence checking is not yet supported for ISO19915")
  expect_error(load_metadata(here::here(bad_dir, "wrong_meta_formats")),
               "Could not determine metadata format")
})

test_that("load_metadata throws an error when there is no xml file with '_metadata' in the name", {
  expect_error(load_metadata(here::here(bad_dir, "no_metadata")),
               "Metadata check failed. No metadata found.")
})

test_that("load_metadata throws an error when there are multiple xml files with '_metadata' in the name", {
  expect_error(load_metadata(here::here(bad_dir, "multiple_xml")),
               "Metadata check failed. The data package format only allows one metadata file per data package.")
})

# ---- test_metadata_version ----
