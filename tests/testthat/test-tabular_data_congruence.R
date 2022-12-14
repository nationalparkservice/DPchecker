# good_dir <- here::here("tests", "testthat", "good")
# bad_dir <- here::here("tests", "testthat", "bad")
good_dir <- "good"
bad_dir <- "bad"

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
                                       "Your EML version is supported")
  expect_message(test_metadata_version(load_metadata(here::here(good_dir, "BUIS_good"))),
                 "Your EML version is supported")

})

test_that("test_metadata_version throws warning when there is a mismatch between EML namespace and schema versions and at least one is too old", {
  expect_warning(
    expect_warning(test_metadata_version(load_metadata(here::here(bad_dir, "bad_versions", "mismatch_warn"))),
               "There is a mismatch"),
               "You are using an old EML version")
})

test_that("test_metadata_version throws warning then error when there is a mismatch between EML namespace and schema versions and at least one is too new", {
  expect_error(
    expect_warning(test_metadata_version(load_metadata(here::here(bad_dir, "bad_versions", "mismatch_error"))),
                   "There is a mismatch"),
    "You are using an unsupported EML version")
})

test_that("test_metadata_version throws error for invalid EML versions", {
  expect_error(test_metadata_version(load_metadata(here::here(bad_dir, "bad_versions", "not_a_version"))))
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
test_that("test_dup_meta_entries displays success message if no duplicate files listed in metadata", {
  expect_message(test_dup_meta_entries(load_metadata(here::here(good_dir, "BICY_good"))),
                 "Each data file name is used exactly once in the metadata file.")
  expect_message(test_dup_meta_entries(load_metadata(here::here(good_dir, "BUIS_good"))),
                 "Each data file name is used exactly once in the metadata file.")

})

test_that("test_dup_meta_entries displays error message if metadata contains duplicate filenames", {
  expect_error(test_dup_meta_entries(load_metadata(here::here(bad_dir,"data_metadata_mismatch", "BICY_files"))),
                 "Metadata file name check failed. Some filenames are used more than once in the metadata:.*Mini_BICY_Veg_Transect_Cleaned.csv.*Mini_BICY_Veg_Intercept_Cleaned.csv$")
  expect_error(test_dup_meta_entries(load_metadata(here::here(bad_dir, "data_metadata_mismatch", "BUIS_files"))),
                 "Metadata file name check failed. Some filenames are used more than once in the metadata:.*BUIS_herps.csv$")

})

# ---- test_file_name_match ----
test_that("test_file_name_match displays success message if files in data dir and metadata match", {
  expect_message(test_file_name_match(here::here(good_dir, "BICY_good")),
                 "All data files are listed in metadata and all metadata files names refer to data files.")
  expect_message(test_file_name_match(here::here(good_dir, "BUIS_good")),
                 "All data files are listed in metadata and all metadata files names refer to data files.")

})

test_that("test_file_name_match displays error message if metadata contains filenames not in data dir and vice versa", {
  expect_error(test_file_name_match(here::here(bad_dir,"data_metadata_mismatch", "BICY_files")),
                 "1 file listed in metadata and missing from data folder.*1 file present in data folder and missing from metadata")
  expect_error(test_file_name_match(here::here(bad_dir, "data_metadata_mismatch", "BUIS_files")),
                 "1 file listed in metadata and missing from data folder.*0 files present in data folder and missing from metadata")

})

# ---- test_fields_match ----
test_that("test_fields_match displays success message if columns in data dir and metadata match", {
  expect_message(test_fields_match(here::here(good_dir, "BICY_good")),
                 "All columns in data match all columns in metadata")
  expect_message(test_fields_match(here::here(good_dir, "BUIS_good")),
                 "All columns in data match all columns in metadata")

})

test_that("test_fields_match displays error message if columns are missing from data/metadata or in wrong order", {
  expect_error(test_fields_match(here::here(bad_dir,"data_metadata_mismatch", "BICY_columns")),
               "Column mismatch between data and metadata\\W*Mini_BICY_Veg_Geography\\.csv\\W*Missing from metadata: decimalLatitude\\W*Mini_BICY_Veg_Intercept_Cleaned\\.csv\\W*Missing from data file: vernacularName\\W*Mini_BICY_Veg_Transect_Cleaned\\.csv\\W*: Metadata column order does not match data column order$")
  expect_error(test_fields_match(here::here(bad_dir, "data_metadata_mismatch", "BUIS_columns")),
               "Column mismatch between data and metadata.*BUIS_herps.csv.*Missing from metadata.*genus.*Missing from data file.*family")

})


# ---- test_numeric_fields ----
test_that("test_numeric_fields displays success message if columns reported as numeric in the metadata can be parsed as numbers", {
  expect_message(test_numeric_fields(here::here(good_dir, "BICY_good")),
                 "Columns indicated as numeric in metadata contain only numeric values and valid missing value codes")
  expect_message(test_numeric_fields(here::here(good_dir, "BUIS_good")),
                 "Columns indicated as numeric in metadata contain only numeric values and valid missing value codes")

})

test_that("test_numeric_fields displays error message if columns reported as numeric in metadata cannot be parsed as numbers", {
  expect_error(test_numeric_fields(here::here(bad_dir,"bad_data_types", "BICY")),
               "Columns indicated as numeric in metadata contain non-numeric values:\\W*Mini_BICY_Veg_Intercept_Cleaned.csv\\W*: custom_Transect\\W*Mini_BICY_Veg_Transect_Cleaned.csv\\W*: individualCount$")
  expect_error(test_numeric_fields(here::here(bad_dir, "bad_data_types", "BUIS")),
               "Columns indicated as numeric in metadata contain non-numeric values:\\W*BUIS_herps.csv\\W*: coordinateUncertaintyInMeters$")
})

# ---- test_date_range ----
test_that("test_date_range displays success message if dates in data match temporal coverage in metadata", {
  expect_message(test_date_range(here::here(good_dir, "BICY_good")),
                 "Columns indicated as date/time in metadata are within the stated temporal coverage range")
  expect_message(test_date_range(here::here(good_dir, "BUIS_good")),
                 "Columns indicated as date/time in metadata are within the stated temporal coverage range.")

})

test_that("test_date_range displays error message if dates in data fail to parse", {
  expect_error(test_date_range(here::here(bad_dir,"bad_data_types", "BICY")),
               "The following date/time columns are out of the range \\W*2002-04-08\\W*2006-06-24\\W* specified in the metadata:\\W*Mini_BICY_Veg_Intercept_Cleaned.csv\\W*: eventDate \\(partially failed to parse\\)\\W*Mini_BICY_Veg_Transect_Cleaned.csv\\W*: eventDate \\(failed to parse\\)$")
})

test_that("test_date_range displays warning message if dates in data are outside temporal coverage range", {
  expect_warning(test_date_range(here::here(bad_dir,"bad_data_types", "BUIS")),
               "The following date/time columns are out of the range \\W*2001-06-20\\W*2001-10-20\\W* specified in the metadata:\\W*BUIS_herps.csv\\W*: eventDate \\W*2001-06-20\\W*2020-10-18\\W*$")
})

# ---- convert_datetime_format ----
test_that("convert_datetime_format returns the correct R datetime format string for ISO compliant date formats", {
  expect_equal(convert_datetime_format("YYYY-MM-DD"), "%Y-%m-%d")
  expect_equal(convert_datetime_format("YYYY-MM-DDThh:mm:ss"), "%Y-%m-%dT%H:%M:%S")
})
