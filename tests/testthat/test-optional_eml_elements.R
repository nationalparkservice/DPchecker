good_dir <- test_path("good")
bad_dir <- test_path("bad")
bicy_meta <- load_metadata(test_path("good", "BICY_good"))
buis_meta <- load_metadata(test_path("good", "BUIS_good"))

# ---- load_metadata ----
test_that("load_metadata works on valid EML file", {
  expect_message(load_metadata(here::here(good_dir, "BICY_good"), inform_success = TRUE),
                 ".*Metadata check passed.*")
  expect_message(load_metadata(here::here(good_dir, "BUIS_good"), inform_success = TRUE),
                 ".*Metadata check passed.*")
})

# ---- test_pii_meta_emails

test_that("test_pii_meta_emails finds true negatives", {
  expect_message(test_pii_meta_emails(here::here(good_dir, "BICY_good")),  "Metadata does not appear to contain any personal emails")
})

# ---- test_notes
test_that("test_notes correctly warns when no notes present", {
  expect_warning(test_notes(metadata = bicy_meta), "Metadata does not contain additionalInfo*")
})


# ---- test_methods
