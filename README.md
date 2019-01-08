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

``` r
## You need to get a bearer token to use this api. 
## You can get this token at: https://fastgenomics.org/ids/Account/ApiTokenLogin?
## BEWARE: Never commit this token to a repository or share it otherwise! It can be used to impersonate you
connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "Bearer Token")
datasets <- fastgenomicsRclient::get_datasets(connection)
print(datasets@content) # all datasets available to you
```

## User permissions

If you get 403 FORBIDDEN errors when using the api, your user account has not the necessary permissions for an operation.
Please contact us at feedback@fastgenomics.org to get access to such functions.


## Run tests

If you want the unit tests of this package, set two envrionment variables

```r
Sys.setenv(FGBEARERTOKEN = "YOUR TOKEN")
Sys.setenv(FGBASEURL = "URL")
```

then run the tests through RStudio (Build -> Test Pakcage) or run devtools::test()
