% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connection.R
\name{connect}
\alias{connect}
\title{Get a FastGenomics connection object to a specific FASTGenomics instance}
\usage{
connect(base_url, email)
}
\arguments{
\item{base_url}{The url of the instance, e.g. https://fastgenomics.org/}

\item{email}{The email address of your account}
}
\value{
a connection object
}
\description{
If you did not provide a Personal Access Token (PAT) for this URL / user combination, you will be prompted to enter your PAT.
The PAT itself is stored in your systems keyring and not in your R History.
WARNINIG: NEVER store your PAT in your R history or in Source Control.
Anybody who has the PAT can take any action on FASTGenomics you can make.
More Questions? Read our \href{https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/authorization_guide.md}{in depth authorization Guide}
}
\examples{
connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "user@example.com")
}
