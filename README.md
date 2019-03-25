# fastgenomicsRclient

Connect to any FASTGenomics instance and manipulate / retrieve data.

## Installation

You can install the development version like this:

``` r
install.packages("devtools")
library(devtools)
install_github("FASTGenomics/r_client")
```

## Example

You need to get a bearer token to use this api.  You can get this
token at: https://prod.fastgenomics.org/webclient/Home/BearerToken/

*WARNING*: Never commit this token to a repository or share it
otherwise! It can be used to impersonate you

``` r
connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Bearer Token")
datasets <- fastgenomicsRclient::get_datasets(connection)
print(datasets@content) # all datasets available to you
```

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

### Compression of submitted data

We internally use the
[`zip`](https://www.rdocumentation.org/packages/utils/versions/3.5.1/topics/zip)
function, which by default uses a system-dependent compression program
(`"zip"` by default).  Make sure to have zip installed if you wan to
submit compressed data.  On Windows one solution is to get it with
[the Rtools package](https://cran.rstudio.com/bin/windows/Rtools/),
just make sure to check the update PATH checkbox when installing it.
