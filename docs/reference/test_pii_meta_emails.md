# Check metadata for PII (emails)

`test_pii_meta_emails()` is a tool to help identify emails in metadata
that may constitute Personally Identifiable Information (PII). This tool
is not guaranteed to find all emails, nor can it definitely tell you
whether an email constitutes PII or not. `test_pii_meta_emails()` reads
in a \*\_metadata.xml file from the specified directory. It uses regular
expressions to extract all emails (in truth, it's hard to test the regex
against all possible emails so there is a chance it will miss one here
or there). If there are no emails in the metadata, the function fails
with a warning (there probably should be an email contact somewhere in
the metadata). If there are any emails that end in anything other than
.gov, the function fails with a warning and lists the offending emails.
If the only emails in metadata end in .gov, these are assumed to be
public emails and the function passes without listing out the emails.

## Usage

``` r
test_pii_meta_emails(directory = here::here())
```

## Arguments

- directory:

  String. The directory where the metadata file resides. Defaults to the
  current working directory.

## Value

invisible(metadata)

## Examples

``` r
if (FALSE) { # \dontrun{
test_pii_meta_emails()
} # }
```
