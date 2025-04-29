good_dir <- testthat::test_path("good")
bad_dir <- testthat::test_path("bad")
bicy_meta <- DPchecker::load_metadata(test_path("good", "BICY_good"))
buis_meta <- DPchecker::load_metadata(test_path("good", "BUIS_good"))

# ---- load_metadata ----
test_that("load_metadata works on valid EML file", {
  expect_message(
    load_metadata(test_path(good_dir, "BICY_good"), inform_success = TRUE),
    ".*Metadata check passed.*"
  )
  expect_message(
    load_metadata(test_path(good_dir, "BUIS_good"), inform_success = TRUE),
    ".*Metadata check passed.*"
  )
})

test_that("load_metadata throws an error for non-EML formats", {
  expect_error(
    load_metadata(test_path(bad_dir, "wrong_meta_formats", "fgdc")),
    "Congruence checking is not yet supported for fgdc"
  )
  expect_error(
    load_metadata(test_path(bad_dir, "wrong_meta_formats", "iso")),
    "Congruence checking is not yet supported for ISO19915"
  )
})

test_that("load_metadata throws an error for non-EML formats", {
  expect_error(
    load_metadata(test_path(bad_dir, "wrong_meta_formats", "fgdc")),
    "Congruence checking is not yet supported for fgdc"
  )
  expect_error(
    load_metadata(test_path(bad_dir, "wrong_meta_formats", "iso")),
    "Congruence checking is not yet supported for ISO19915"
  )
  expect_error(
    load_metadata(test_path(bad_dir, "wrong_meta_formats")),
    "Could not determine metadata format"
  )
})

test_that("load_metadata throws an error when there is no xml file with '_metadata' in the name", {
  expect_error(
    load_metadata(test_path(bad_dir, "no_metadata")),
    "Metadata check failed. No metadata found."
  )
})

test_that("load_metadata throws an error when there are multiple xml files with '_metadata' in the name", {
  expect_error(
    load_metadata(test_path(bad_dir, "multiple_xml")),
    "Metadata check failed. The data package format only allows one metadata file per data package."
  )
})

# ---- run_congruence_checks ----
# cli::test_that_cli("run_congruence_checks works", configs = "plain", {
#  expect_error(run_congruence_checks(test_path(bad_dir, "BICY_bad")),
#               "Metadata schema must validate")
#  expect_error(run_congruence_checks(test_path(bad_dir, "data_metadata_mismatch", "BICY_files")),
#               "You must remove duplicate data table names")
#  expect_snapshot(run_congruence_checks(test_path(good_dir, "BICY_good")))
#  expect_snapshot(run_congruence_checks(test_path(good_dir, "BICY_good"), #check_metadata_only = TRUE))
# })

# ---- test_metadata_version ----
test_that("test_metadata_version displays success message for supported EML versions", {
  expect_message(
    test_metadata_version(load_metadata(test_path(good_dir, "BICY_good"))),
    "Your EML version is supported"
  )
  expect_message(
    test_metadata_version(load_metadata(test_path(good_dir, "BUIS_good"))),
    "Your EML version is supported"
  )
})

test_that("test_metadata_version throws warning when there is a mismatch between EML namespace and schema versions and at least one is too old", {
  expect_warning(
    expect_warning(
      test_metadata_version(load_metadata(test_path(bad_dir, "bad_versions", "mismatch_warn"))),
      "There is a mismatch"
    ),
    "You are using an old EML version"
  )
})

test_that("test_metadata_version throws warning then error when there is a mismatch between EML namespace and schema versions and at least one is too new", {
  expect_error(
    expect_warning(
      test_metadata_version(load_metadata(test_path(bad_dir, "bad_versions", "mismatch_error"))),
      "There is a mismatch"
    ),
    "You are using an unsupported EML version"
  )
})

test_that("test_metadata_version throws error for invalid EML versions", {
  expect_error(test_metadata_version(load_metadata(test_path(bad_dir, "bad_versions", "not_a_version"))))
})


# ---- test_validate_schema ----
test_that("test_validate_schema displays success message for valid schema", {
  expect_message(
    test_validate_schema(load_metadata(test_path(good_dir, "BICY_good"))),
    "Your metadata is schema valid."
  )
  expect_message(
    test_validate_schema(load_metadata(test_path(good_dir, "BUIS_good"))),
    "Your metadata is schema valid."
  )
})

test_that("test_validate_schema throws error if eml validation returns errors or warnings", {
  expect_error(
    test_validate_schema(load_metadata(test_path(bad_dir, "BICY_bad"))),
    "Your metadata is schema-invalid."
  )
})

# ---- test_footer ----
test_that("test_footer displays success message if metadata indicates no footer", {
  expect_message(
    test_footer(load_metadata(test_path(good_dir, "BICY_good"))),
    "Metadata indicates data files do not have footers."
  )
  expect_message(
    test_footer(load_metadata(test_path(good_dir, "BUIS_good"))),
    "Metadata indicates data files do not have footers."
  )
})

test_that("test_footer throws error if metadata indicates footers present", {
  expect_error(
    test_footer(load_metadata(test_path(bad_dir, "BUIS_bad"))),
    "Metadata indicates that data files include footers. Please remove all footers from data files."
  )
})
# ---- test_header_num ----
test_that("test_header_num displays success message if metadata indicates exactly one header row per file", {
  expect_message(
    test_header_num(load_metadata(test_path(good_dir, "BICY_good"))),
    "Metadata indicates that each data file contains exactly one header row."
  )
  expect_message(
    test_header_num(load_metadata(test_path(good_dir, "BUIS_good"))),
    "Metadata indicates that each data file contains exactly one header row."
  )
})

test_that("test_header_num throws error if metadata does not contain header info", {
  expect_error(
    test_header_num(load_metadata(test_path(bad_dir, "BUIS_bad"))),
    "Metadata does not contain information about number of header rows"
  )
})

test_that("test_header_num throws error if metadata indicates wrong number of header rows", {
  expect_error(
    test_header_num(load_metadata(test_path(bad_dir, "BICY_bad"))),
    "Metadata indicates that the following data files contain either zero or more than one header row:"
  )
})

# ---- test_delimiter ----
test_that("test_delimiter displays success message if metadata indicates that data files contain single delimiter", {
  expect_message(
    test_delimiter(load_metadata(test_path(good_dir, "BICY_good"))),
    "Metadata indicates that each data file contains a field delimiter that is a single character"
  )
  expect_message(
    test_delimiter(load_metadata(test_path(good_dir, "BUIS_good"))),
    "Metadata indicates that each data file contains a field delimiter that is a single character"
  )
})

test_that("test_delimiter throws error if metadata does not contain delimiter info", {
  expect_error(
    test_delimiter(load_metadata(test_path(bad_dir, "BUIS_bad"))),
    "Metadata does not contain information about the field delimiter"
  )
})

test_that("test_delimiter throws error if metadata indicates delimiter with zero or multiple characters", {
  expect_error(
    test_delimiter(load_metadata(test_path(bad_dir, "BICY_bad"))),
    "Metadata indicates that the following data files do not contain valid delimiters:.*Example Intercept Observations.*Example Transect Observations$"
  )
})

# ---- test_dup_meta_entries ----
test_that("test_dup_meta_entries displays success message if no duplicate files listed in metadata", {
  expect_message(
    test_dup_meta_entries(load_metadata(test_path(good_dir, "BICY_good"))),
    "Each data file name is used exactly once in the metadata file."
  )
  expect_message(
    test_dup_meta_entries(load_metadata(test_path(good_dir, "BUIS_good"))),
    "Each data file name is used exactly once in the metadata file."
  )
})

test_that("test_dup_meta_entries displays error message if metadata contains duplicate filenames", {
  expect_error(
    test_dup_meta_entries(load_metadata(test_path(bad_dir, "data_metadata_mismatch", "BICY_files"))),
    "Metadata file name check failed. Some filenames are used more than once in the metadata:.*Mini_BICY_Veg_Transect_Cleaned.csv.*Mini_BICY_Veg_Intercept_Cleaned.csv$"
  )
  expect_error(
    test_dup_meta_entries(load_metadata(test_path(bad_dir, "data_metadata_mismatch", "BUIS_files"))),
    "Metadata file name check failed. Some filenames are used more than once in the metadata:.*BUIS_herps.csv$"
  )
})

# ---- test_datatable_urls ----

test_that("test_datatable_urls displays success message if all datatables have urls", {
  expect_message(test_datatable_urls(load_metadata(test_path("good", "BICY_good"))), "Metadata contains URLs for all data tables.")})

# ---- test_datatable_urls_doi ----

test_that("test_datatable_urls_doi displays success message if datatable urls have the correct format for the doi", {
  expect_message(test_datatable_urls_doi(load_metadata(test_path("good", "BICY_good"))), "Data table URLs are properly formmatted and correspond to the specified DOI.")
})

# ---- test_datatable_url_attributes ---

test_that("test_datatable_url_attributes displays appropriate warning if no attribute info", {
  expect_warning(test_datatable_url_attributes(load_metadata(test_path("good", "BICY_good"))), "One or more of data file URLs elements in metadata lack attributes.")
})

test_that("test_datatable_url_attributes displays success when proper attribute and url are supplied", {
  meta <- DPchecker::load_metadata(test_path("good", "BICY_good"))
  meta2 <- EMLeditor::set_data_urls(meta, tag = "information")
  expect_message(test_datatable_url_attributes(meta2), "Metadata datatable URLs and URL attributes are properly specified.")
})

test_that("test_datatable_url_attributes displays warning when attribute and url do not coincide", {
  meta <- DPchecker::load_metadata(test_path("good", "BICY_good"))
  meta2 <- EMLeditor::set_data_urls(meta,
                                    url = "test.com",
                                    tag = "download")
  expect_warning(test_datatable_url_attributes(meta2), "One or more data file URL attributes in metadata are set to \"download\".")
})


# ---- test_file_name_match ----
test_that("test_file_name_match displays success message if files in data dir and metadata match", {
  expect_message(
    test_file_name_match(test_path(good_dir, "BICY_good")),
    "All data files are listed in metadata and all metadata files names refer to data files."
  )
  expect_message(
    test_file_name_match(test_path(good_dir, "BUIS_good")),
    "All data files are listed in metadata and all metadata files names refer to data files."
  )
})

test_that("test_file_name_match displays error message if metadata contains filenames not in data dir and vice versa", {
  expect_error(
    test_file_name_match(test_path(bad_dir, "data_metadata_mismatch", "BICY_files")),
    "1 file listed in metadata but missing from data folder.*1 file present in data folder but missing from metadata"
  )
  expect_error(
    test_file_name_match(test_path(bad_dir, "data_metadata_mismatch", "BUIS_files")),
    "1 file listed in metadata but missing from data folder."
  )
})

# ---- test_fields_match ----
test_that("test_fields_match displays success message if columns in data dir and metadata match", {
  expect_message(
    test_fields_match(test_path(good_dir, "BICY_good")),
    "All columns in data match all columns in metadata"
  )
  expect_message(
    test_fields_match(test_path(good_dir, "BUIS_good")),
    "All columns in data match all columns in metadata"
  )
})

test_that("test_fields_match displays error message if columns are missing from data/metadata or in wrong order", {
  expect_error(
    test_fields_match(test_path(bad_dir, "data_metadata_mismatch", "BICY_columns")),
    "Column mismatch between data and metadata\\W*Mini_BICY_Veg_Geography\\.csv\\W*Missing from metadata: decimalLatitude\\W*Mini_BICY_Veg_Intercept_Cleaned\\.csv\\W*Missing from data file: vernacularName\\W*Mini_BICY_Veg_Transect_Cleaned\\.csv\\W*: Metadata column order does not match data column order$"
  )
  expect_error(
    test_fields_match(test_path(bad_dir, "data_metadata_mismatch", "BUIS_columns")),
    "Column mismatch between data and metadata.*BUIS_herps.csv.*Missing from metadata.*genus.*Missing from data file.*family"
  )
})


# ---- test_numeric_fields ----
test_that("test_numeric_fields displays success message if columns reported as numeric in the metadata can be parsed as numbers", {
  expect_message(
    test_numeric_fields(test_path(good_dir, "BICY_good")),
    "Columns indicated as numeric in metadata contain only numeric values and valid missing value codes"
  )
  expect_message(
    test_numeric_fields(test_path(good_dir, "BUIS_good")),
    "Columns indicated as numeric in metadata contain only numeric values and valid missing value codes"
  )
})

test_that("test_numeric_fields displays error message if columns reported as numeric in metadata cannot be parsed as numbers", {
  expect_error(
    test_numeric_fields(test_path(bad_dir, "bad_data_types", "BICY")),
    "Columns indicated as numeric in metadata contain non-numeric values:\\W*Mini_BICY_Veg_Intercept_Cleaned.csv\\W*: custom_Transect\\W*Mini_BICY_Veg_Transect_Cleaned.csv\\W*: individualCount$"
  )
  expect_error(
    test_numeric_fields(test_path(bad_dir, "bad_data_types", "BUIS")),
    "Columns indicated as numeric in metadata contain non-numeric values:\\W*BUIS_herps.csv\\W*: coordinateUncertaintyInMeters$"
  )
})

# ---- test_missing_data ----
test_that("test_missing_data displays file-level error message", {
  expect_error(
    test_missing_data(test_path(good_dir, "BICY_good")),
    "Undocumented missing data detected. Please document all missing data in metadata:\\W*Mini_BICY_Veg_Intercept_Cleaned.csv\\W*contains missing data without a corresponding\\W*missing data code in metadata.\\W*Mini_BICY_Veg_Transect_Cleaned.csv\\W*contains missing data without a corresponding\\W*missing data code in metadata."
  )
})

test_that("test_mising_data displays test pass message at column levels", {
  expect_message(
    test_missing_data(test_path(good_dir, "BUIS_good"),
      detail_level = "columns"
    ),
    'Missing data listed as NA, "blank", or "empty" is accounted for in metadata'
  )
})




# ---- test_date_range ----
# test_that("test_date_range displays success message if dates in data match temporal coverage in metadata", {
#  expect_message(test_date_range(test_path(good_dir, "BICY_good")),
#                 "Columns indicated as date/time in metadata are within the stated temporal coverage range")
#  expect_message(test_date_range(test_path(good_dir, "BUIS_good")),
#                 "Columns indicated as date/time in metadata are within the stated temporal coverage range.")

# })

# test_that("test_date_range displays error message if dates in data fail to parse", {
#  expect_error(test_date_range(test_path(bad_dir,"bad_data_types", "BICY")),
#               "The following date/time columns are out of the range \\W*2002-04-08\\W*2006-06-24\\W* specified in the metadata. To exclude QA/QC dates, re-run \\W*DPchecker::run_congruence_checks\\(\\)\\W* with skip_cols set to the columns to skip.\\W*Mini_BICY_Veg_Intercept_Cleaned.csv\\W*: eventDate \\(partially failed to parse\\)\\W*Mini_BICY_Veg_Transect_Cleaned.csv\\W*: eventDate \\(failed to parse\\)$")
# })

# test_that("test_date_range displays warning message if dates in data are outside temporal coverage range", {
# bad_dir <- test_path("bad")
#  expect_warning(suppressWarnings(test_date_range(test_path(bad_dir,"bad_data_types", "BUIS"))),
#               "The following date/time columns are out of the range \\W*2001-06-20\\W*2001-10-20\\W* specified in the metadata. To exclude QA/QC dates, re-run \\W*DPchecker::run_congruence_checks\\(\\)\\W* with skip_cols set to the columns to skip.\\W*BUIS_herps.csv\\W*: eventDate \\W*2001-06-20\\W*2020-10-18\\W*$")
# })

# ---- test_taxonomic_cov ----
test_that("test_taxonomic_cov displays success message if taxonomic coverage element is present", {
  expect_message(
    test_taxonomic_cov(load_metadata(test_path(good_dir, "BICY_good"))),
    "Metadata contains taxonomic coverage element"
  )
  expect_message(
    test_taxonomic_cov(load_metadata(test_path(good_dir, "BUIS_good"))),
    "Metadata contains taxonomic coverage element"
  )
})

test_that("test_taxonomic_cov throws warning if taxonomic coverage element is missing", {
  bicy_meta$dataset$coverage$taxonomicCoverage <- NULL
  buis_meta$dataset$coverage$taxonomicCoverage <- NULL
  expect_warning(
    test_taxonomic_cov(bicy_meta),
    "Metadata does not contain taxonomic coverage information"
  )
  expect_warning(
    test_taxonomic_cov(buis_meta),
    "Metadata does not contain taxonomic coverage information"
  )
})

# ---- test_geographic_cov ----
test_that("test_geographic_cov displays success message if geographic coverage element is present", {
  expect_message(
    test_geographic_cov(load_metadata(test_path(good_dir, "BICY_good"))),
    "Metadata contains geographic coverage element"
  )
  expect_message(
    test_geographic_cov(load_metadata(test_path(good_dir, "BUIS_good"))),
    "Metadata contains geographic coverage element"
  )
})

test_that("test_geographic_cov throws warning if geographic coverage element is missing", {
  bicy_meta$dataset$coverage$geographicCoverage <- NULL
  buis_meta$dataset$coverage$geographicCoverage <- NULL
  expect_warning(
    test_geographic_cov(bicy_meta),
    "Metadata does not contain geographic coverage information"
  )
  expect_warning(
    test_geographic_cov(buis_meta),
    "Metadata does not contain geographic coverage information"
  )
})

# ---- test_doi ----
test_that("test_doi displays success message if DOI is present", {
  expect_message(
    test_doi(load_metadata(test_path(good_dir, "BICY_good"))),
    "Metadata contains a digital object identifier"
  )
  expect_message(
    test_doi(load_metadata(test_path(good_dir, "BUIS_good"))),
    "Metadata contains a digital object identifier"
  )
})

test_that("test_doi throws warning if DOI is missing", {
  bicy_meta$dataset$alternateIdentifier <- NULL
  buis_meta$dataset$alternateIdentifier <- NULL
  expect_warning(
    test_doi(bicy_meta),
    "Metadata does not contain a digital object identifier"
  )
  expect_warning(
    test_doi(buis_meta),
    "Metadata does not contain a digital object identifier"
  )
})

# ---- test_publisher ----
test_that("test_publisher displays success message if publisher element is present", {
  expect_message(
    test_publisher(load_metadata(test_path(good_dir, "BICY_good"))),
    "Metadata contains publisher element"
  )
  expect_message(
    test_publisher(load_metadata(test_path(good_dir, "BUIS_good"))),
    "Metadata contains publisher element"
  )
})

test_that("test_publisher throws error if publisher element is missing", {
  bicy_meta$dataset$publisher <- NULL
  buis_meta$dataset$publisher <- NULL
  expect_error(
    test_publisher(bicy_meta),
    "Metadata does not contain publisher information"
  )
  expect_error(
    test_publisher(buis_meta),
    "Metadata does not contain publisher information"
  )
})

# ---- test_valid_filenames ----
test_that("test_valid_filenames displays success message if filenames do not contain special characters", {
  expect_message(
    test_valid_filenames(load_metadata(test_path(good_dir, "BICY_good"))),
    "File names begin with a letter and do not contain spaces or special characters"
  )
  expect_message(
    test_valid_filenames(load_metadata(test_path(good_dir, "BUIS_good"))),
    "File names begin with a letter and do not contain spaces or special characters"
  )
})

test_that("test_valid_filenames throws warning if filenames contain special characters", {
  bicy_meta$dataset$dataTable[[2]]$physical$objectName <- "0ops_bad_filename.csv"
  buis_meta$dataset$dataTable$physical$objectName <- "als*o bad.csv"
  expect_warning(
    test_valid_filenames(bicy_meta),
    "Some file names contain special characters and/or do not begin with a letter"
  )
  expect_warning(
    test_valid_filenames(buis_meta),
    "Some file names contain special characters and/or do not begin with a letter"
  )
})

# ---- test_valid_fieldnames ----
test_that("test_valid_fieldnames displays success message if filenames do not contain special characters", {
  expect_message(
    test_valid_fieldnames(load_metadata(test_path(good_dir, "BICY_good"))),
    "Field names begin with a letter and do not contain spaces or special characters"
  )
  expect_message(
    test_valid_fieldnames(load_metadata(test_path(good_dir, "BUIS_good"))),
    "Field names begin with a letter and do not contain spaces or special characters"
  )
})

test_that("test_valid_fieldnames throws warning if filenames contain special characters", {
  bicy_meta$dataset$dataTable[[2]]$attributeList$attribute[[3]]$attributeName <- "_weird_col_name"
  buis_meta$dataset$dataTable$attributeList$attribute[[4]]$attributeName <- "poor choices were made!"
  expect_warning(
    test_valid_fieldnames(bicy_meta),
    "Some field names contain special characters and/or do not begin with a letter"
  )
  expect_warning(
    test_valid_fieldnames(buis_meta),
    "Some field names contain special characters and/or do not begin with a letter"
  )
})

# ---- convert_datetime_format ----
test_that("convert_datetime_format returns the correct R datetime format string for ISO compliant date formats", {
  expect_equal(convert_datetime_format("YYYY-MM-DD"), "%Y-%m-%d")
  expect_equal(convert_datetime_format("YYYY-MM-DDThh:mm:ss"), "%Y-%m-%dT%H:%M:%S")
})
