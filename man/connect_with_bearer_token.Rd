% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connection.R
\name{connect_with_bearer_token}
\alias{connect_with_bearer_token}
\title{Get a FastGenomics connection object to a specific FASTGenomics instance}
\usage{
connect_with_bearer_token(base_url, email, bearer_token)
}
\arguments{
\item{base_url}{The url of the instance, e.g. https://fastgenomics.org/}

\item{email}{The email address of your account}

\item{bearer_token}{Your Bearer Token, NEVER share this or store it in your history etc. Should look like 'Bearer ey'}
}
\value{
a connection object
}
\description{
This methods uses bearer tokens. Good for demonstrations but needs frequent manual refreshment of the bearer token. Prefer connect to use a PAT.
If you use this method, please delete the FgConnection object when you are done.
More Questions? Read our \href{https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/authorization_guide.md}{in depth authorization Guide}
}
\examples{
connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "user@example.com", "Bearer ey...")
}
