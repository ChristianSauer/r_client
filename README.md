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
The connection object itself does not contain your PAT (this is actually stored on your PCs keyring), so if you compromise it, it is not as critical. Any connection object will expire in a few ours.

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