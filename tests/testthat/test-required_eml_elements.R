good_dir <- testthat::test_path("good")
bad_dir <- testthat::test_path("bad")
bicy_meta <- DPchecker::load_metadata(test_path("good", "BICY_good"))
buis_meta <- DPchecker::load_metadata(test_path("good", "BUIS_good"))

# ---- test pub date 
test_that("test_pub_date works on good pub dates", {
  msg <- "Publication date, 2022, predates the Data Package Reference Type."
  expect_warning(
    test_pub_date(metadata = bicy_meta),
    msg)
})


