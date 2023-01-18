# run_congruence_checks works [plain]

    Code
      run_congruence_checks(here::here(good_dir, "BICY_good"))
    Message <cliMessage>
      
      -- Running all congruence checks -----------------------------------------------
      
      -- Checking metadata compliance --
      
    Message <rlang_message>
      v Your metadata is schema valid.
      v Each data file name is used exactly once in the metadata file.
      v Your EML version is supported.
      v Metadata indicates that each data file contains a field delimiter that is a single character
      v Metadata indicates that each data file contains exactly one header row.
      v Metadata indicates data files do not have footers.
    Message <cliMessage>
      
      -- Checking that metadata is consistent with data file(s) --
      
    Message <rlang_message>
      v All data files are listed in metadata and all metadata files names refer to data files.
      v All columns in data match all columns in metadata.
      v Columns indicated as numeric in metadata contain only numeric values and valid missing value codes.
      v Columns indicated as date/time in metadata are within the stated temporal coverage range.
    Message <cliMessage>
      
      -- Summary --
      
      v Success! All congruence checks passed.

---

    Code
      run_congruence_checks(here::here(good_dir, "BICY_good"), check_metadata_only = TRUE)
    Message <cliMessage>
      
      -- Running metadata-only checks (skipping checks against data files) -----------
      
      -- Checking metadata compliance --
      
    Message <rlang_message>
      v Your metadata is schema valid.
      v Each data file name is used exactly once in the metadata file.
      v Your EML version is supported.
      v Metadata indicates that each data file contains a field delimiter that is a single character
      v Metadata indicates that each data file contains exactly one header row.
      v Metadata indicates data files do not have footers.
    Message <cliMessage>
      
      -- Summary --
      
      v Success! All metadata checks passed.

