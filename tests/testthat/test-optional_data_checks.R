good_dir <- test_path("good")
bad_dir <- test_path("bad")
bicy_meta <- load_metadata(test_path("good", "BICY_good"))
buis_meta <- load_metadata(test_path("good", "BUIS_good"))

# ---- test_pii_meta_emails

test_that("test_pii_data_emails finds true negatives",
          {
            msg <- "Data files do not appear to contain any personal emails."
            expect_message(test_pii_data_emails(here::here(good_dir,
                                                           "BICY_good")),
                           msg)
          }
)

# ----
