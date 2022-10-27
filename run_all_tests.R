dirs <- list.dirs(here::here("scratchpad"))
dirs <- dirs[-1]

for (dir in dirs) {
  cat(paste(crayon::blue$bold(basename(dir)), "\n"))

  meta <- try(load_metadata(dir))
  data <- try(load_data(dir))

  if (!any("try-error" %in% class(meta))) {
    try(test_validate_schema(meta))
    try(test_metadata_version(meta))
    try(test_delimiter(meta))
    try(test_footer(meta))
    try(test_header_num(meta))
    try(test_dup_data_files(dir))
    try(test_dup_meta_entries(meta))
    try(test_file_name_match(dir, meta))
    try(test_fields_match(dir, meta))
  }

  cat("\n")
}
