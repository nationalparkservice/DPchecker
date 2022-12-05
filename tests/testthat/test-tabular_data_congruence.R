good_dir <- here::here("tests", "testthat", "good")
bad_dir <- here::here("tests", "testthat", "bad")

# ---- load_metadata ----

test_that("load_metadata works on valid EML file", {
  expect_message(load_metadata(here::here(good_dir, "BICY_good"), inform_success = TRUE),
                 ".*Metadata check passed.*")
  expect_message(load_metadata(here::here(good_dir, "BUIS_good"), inform_success = TRUE),
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
test_that("test_metadata_version displays success message for supported EML versions", {
  expect_message(test_metadata_version(load_metadata(here::here(good_dir, "BICY_good"))),
                                       "Your EML version 2.2.0 is supported.")
  expect_message(test_metadata_version(load_metadata(here::here(good_dir, "BUIS_good"))),
                 "Your EML version 2.2.3 is supported.")

})

test_that("test_metadata_version throws error and displays failure message for unsupported EML versions", {
  expect_error(test_metadata_version(load_metadata(here::here(bad_dir, "BICY_bad"))),
                 "Unsupported EML version: EML must be 2.2.0 or later. Your version is 2.1.0.")
})

test_that("test_metadata_version throws error for invalid EML versions", {
  expect_error(test_metadata_version(load_metadata(here::here(bad_dir, "BUIS_bad"))))
})


# ---- test_validate_schema ----
test_that("test_validate_schema displays success message for valid schema", {
  expect_message(test_validate_schema(load_metadata(here::here(good_dir, "BICY_good"))),
                 "Your metadata is schema valid.")
  expect_message(test_validate_schema(load_metadata(here::here(good_dir, "BUIS_good"))),
                 "Your metadata is schema valid.")

})

test_that("test_validate_schema throws error if eml validation returns errors or warnings", {
  expect_error(test_validate_schema(load_metadata(here::here(bad_dir, "BICY_bad"))),
               "Your metadata is schema-invalid.")
})

# ---- test_footer ----
test_that("test_footer displays success message if metadata indicates no footer", {
  expect_message(test_footer(load_metadata(here::here(good_dir, "BICY_good"))),
                 "Metadata indicates data files do not have footers.")
  expect_message(test_footer(load_metadata(here::here(good_dir, "BUIS_good"))),
                 "Metadata indicates data files do not have footers.")

})

test_that("test_footer throws error if metadata indicates footers present", {
  expect_error(test_footer(load_metadata(here::here(bad_dir, "BUIS_bad"))),
               "Metadata indicates that data files include footers. Please remove all footers from data files.")
})
# ---- test_header_num ----
test_that("test_header_num displays success message if metadata indicates exactly one header row per file", {
  expect_message(test_header_num(load_metadata(here::here(good_dir, "BICY_good"))),
                 "Metadata indicates that each data file contains exactly one header row.")
  expect_message(test_header_num(load_metadata(here::here(good_dir, "BUIS_good"))),
                 "Metadata indicates that each data file contains exactly one header row.")

})

test_that("test_header_num throws error if metadata does not contain header info", {
  expect_error(test_header_num(load_metadata(here::here(bad_dir, "BUIS_bad"))),
               "Metadata does not contain information about number of header rows")
})

test_that("test_header_num throws error if metadata indicates wrong number of header rows", {
  expect_error(test_header_num(load_metadata(here::here(bad_dir, "BICY_bad"))),
               "Metadata indicates that the following data files contain either zero or more than one header row:")
})

# ---- test_delimiter ----
test_that("test_delimiter displays success message if metadata indicates that data files contain single delimiter", {
  expect_message(test_delimiter(load_metadata(here::here(good_dir, "BICY_good"))),
                 "Metadata indicates that each data file contains a field delimiter that is a single character")
  expect_message(test_delimiter(load_metadata(here::here(good_dir, "BUIS_good"))),
                 "Metadata indicates that each data file contains a field delimiter that is a single character")

})

test_that("test_delimiter throws error if metadata does not contain delimiter info", {
  expect_error(test_delimiter(load_metadata(here::here(bad_dir, "BUIS_bad"))),
               "Metadata does not contain information about the field delimiter")
})

test_that("test_delimiter throws error if metadata indicates delimiter with zero or multiple characters", {
  expect_error(test_delimiter(load_metadata(here::here(bad_dir, "BICY_bad"))),
               "Metadata indicates that the following data files do not contain valid delimiters:.*Example Intercept Observations.*Example Transect Observations$")
})
# ---- test_dup_meta_entries ----
# ---- test_dup_data_files ----
# ---- test_file_name_match ----
# ---- test_fields_match ----
# ---- test_numeric_fields ----
# ---- test_date_range ----
# ---- convert_datetime_format ----
