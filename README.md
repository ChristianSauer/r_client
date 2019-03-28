# fastgenomicsRclient

Connect to any FASTGenomics instance and manipulate / retrieve data.

## Installation

You can install the development version like this:

*Hint* devtools needs some system packages to work, see this page for [details](https://www.r-project.org/nosvn/pandoc/devtools.html). The exact steps vary for each Linux distribution so your best bet is to search for "devtools r install your linix distribution".

*Hint* We need libsodium, this can be installed like this on Ubuntu:

``` bash
sudo apt-get install libsodium-dev
```

``` r
install.packages("devtools")
library(devtools)
install_github("FASTGenomics/r_client")
```

## Example

First you need to authorize yourself. This packlage currently only supports login via Personal Access Tokens (PATs). See  our [Authorization Guide](https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/authorization%20guide.md) for more details.

1. Create a PAT for your account [here](https://prod.fastgenomics.org/ids/Manage/NewPatToken)
1. Save the created PAT somewhere.

If you need more guidance, please read our [Authorization Guide](https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/authorization%20guide.md).

``` r
fastgenomicsRclient::save_personal_access_token("https://fastgenomics.org/", "user@example.com") # this will show a display where you can enter your PAT
connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "user@example.com")
datasets <- fastgenomicsRclient::get_datasets(connection)
print(datasets@content) # all datasets available to you
```

**Warning** Never store your PAT in a variable or somewhere where it can be compromised. Anybody with your PAT can modify your data as he sees fit. If your PAT is compromised, [revoke it](https://prod.fastgenomics.org/ids/Manage/ManagePats?)!
The connection object should also not be shared, it contains your PAT.

## Run tests

If you want the unit tests of this package, set these environment variables
Note: You can obtain a PAT (Personal access token) using this [guide](https://github.com/FASTGenomics/fastgenomics-docs/blob/releases/next/doc/api/authorization%20guide.md)
In no circumstances, share your PAT with anyone or commit it to source control!

```r
Sys.setenv(FGBASEURL="URL", FG_EMAIL="TEST_ACCOUNT", FG_PAT="PAT")
```

then run the tests through RStudio (Build -> Test Pakcage) or run
`devtools::test()`

## Troubleshooting

### HTTP2 framing error

If you encounter an HTTP2 framing error try disabling HTTP2 with

``` r
library("httr")
httr::set_config(config(http_version = 0))
```

### User permissions

If you get 403 FORBIDDEN errors when using the API, your user account
has not the necessary permissions for an operation.  Please contact us
at feedback@fastgenomics.org to get access to such functions.

### Jupyter notebook compatibility

Due to a known bug in R kernel for jupyter the R session is [not considered
interactive](https://github.com/IRkernel/IRkernel/issues/236) (`interactive()` returns
`FALSE`).  This causes the password prompts used by the keyring package (either
`askpass` or `getPass` depending on the version of keyring) to return `NULL` resulting
in subsequent cryptic errors from keyring.  Because of that the r_client does not run in
jupyter notebook/lab.

### Keyring backends

If for some reason the default backend of the keyring package does not work for you, you
can switch to a different backend by specifying

``` r
options(keyring_backend = "env")
```

You can find the list of available backends
[here](https://github.com/r-lib/keyring#configuring-an-os-specific-backend).
