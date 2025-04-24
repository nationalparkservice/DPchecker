good_dir <- test_path("good")
bad_dir <- test_path("bad")
bicy_meta <- load_metadata(test_path("good", "BICY_good"))
buis_meta <- load_metadata(test_path("good", "BUIS_good"))

# ---- test_pii_meta_emails

test_that("test_pii_meta_emails finds true negatives", {
  msg <- "Metadata does not appear to contain any personal emails"
  expect_message(
    test_pii_meta_emails(test_path(
      good_dir,
      "BICY_good"
    )),
    msg
  )
})

# ---- test_notes
test_that("test_notes correctly warns when no notes present", {
  expect_warning(
    test_notes(metadata = bicy_meta),
    "Metadata does not contain additionalInfo*"
  )
})


# ---- test_methods

test_that("test_methods identifies presence of non-standard characters", {
  msg <- paste0(
    "The metadata methods contains ",
    "non-standard characters such as*"
  )
  expect_warning(
    test_methods(metadata = bicy_meta),
    msg
  )
})

# ---- test_orcid_exists

test_that("test_orcid_exists correctly identifies orcids exist", {
  expect_message(
    test_orcid_exists(metadata = bicy_meta),
    "All individual creators have associated ORCiDs."
  )
})

# ---- test_orcid_format

test_that("test_orcid_format identifies properly formatted orcids", {
  expect_message(
    test_orcid_format(metadata = bicy_meta),
    "All Creator ORCiDs are properly formatted."
  )
})

# ----- test_orcid_resolves ----
test_that("test_orcid_resolves correctly identifies orcids that resolve", {
  msg <- "All Creator ORCiDs resolved to a valid ORCiD profile."
  expect_message(
    test_orcid_resolves(metadata = bicy_meta),
    msg
  )
})

# ---- test_orcid_match
test_that("test_orcid_match correctly identifies when orcid and crearot match", {
  msg <- paste0(
    "All Creator ORCiDs resolve to an ORCiD profile ",
    "that matches the Creator last name."
  )
  expect_message(
    test_orcid_match(metadata = bicy_meta),
    msg
  )
})

# ---- test_project
test_that("test_project warns when no project is present", {
  expect_warning(
    test_project(metadata = bicy_meta),
    "No project associated with the metadata.*"
  )
})

# ---- test_content_units
test_that("test_content_units warns when no content units links are present", {
  expect_warning(
    test_content_units(metadata = bicy_meta),
    "Metadata does not contain park content unit*"
  )
})

test_that("test_content_units passes when content unit links are present", {
  meta2 <- EMLeditor::set_content_units(bicy_meta, "ROMO")
  expect_message(
    test_content_units(meta2),
    "Metadata contains NPS content unit links."
  )
})

test_that("test_content_units warns when no geography present", {
  meta <- bicy_meta
  meta[["dataset"]][["coverage"]][["geographicCoverage"]] <- NULL
  expect_warning(
    test_content_units(metadata = meta),
    "Metadata does not contain park content unit links.*"
  )
})
