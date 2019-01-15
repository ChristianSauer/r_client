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

TODO

``` r
## You need to get a bearer token to use this api.
## You can get this token at: https://fastgenomics.org/ids/Account/ApiTokenLogin?
## BEWARE: Never commit this token to a repository or share it otherwise! It can be used to impersonate you
connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Bearer Token")
datasets <- fastgenomicsRclient::get_datasets(connection)
print(datasets@content) # all datasets available to you
```

## Run tests

If you want the unit tests of this package, set two envrionment variables

```r
Sys.setenv(BEARERTOKEN = "YOUR TOKEN")
Sys.setenv(BASEURL = "URL")
```

## Troubleshooting

If you encounter an HTTP2 framing error try disabling HTTP2 with

``` r
library("httr")
httr::set_config(config(http_version = 0))
```
