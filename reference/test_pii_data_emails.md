# Check data files for PII (emails)

`test_pii_data_emails()` is a tool to help identify emails in the data
files (.csv) that may constitute Personally Identifiable Information
(PII). This tool is not guaranteed to find all emails, nor can it
definitely tell you whether an email constitutes PII or not.
`test_pii_data_emails()` reads in each .csv file from the specified
directory. It uses regular expressions to extract all emails (in truth,
it's hard to test the regex against all possible emails so there is a
chance it will miss one here or there). If there are any emails that end
in anything other than .gov, the function fails with a warning and lists
the offending files and the emails they contain. If there are no emails
in the data files or the only emails in the data files end in .gov,
these are assumed to be public emails and the function passes without
listing any emails.

## Usage

``` r
test_pii_data_emails(directory = here::here())
```

## Arguments

- directory:

  String. The directory where the data package resides. Defaults to the
  current working directory.

## Value

String

## Examples

``` r
if (FALSE) { # \dontrun{
test_pii_data_emails()
} # }
```
