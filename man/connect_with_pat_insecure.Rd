% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connection.R
\name{connect_with_pat_insecure}
\alias{connect_with_pat_insecure}
\title{Get a FastGenomics connection object to a specific FASTGenomics instance.
This method is less secure than using connect since the PAT will end in your R History}
\usage{
connect_with_pat_insecure(base_url, email, pat)
}
\arguments{
\item{base_url}{The url of the instance, e.g. https://fastgenomics.org/}

\item{email}{The email address of your account}

\item{pat}{Your PAT, NEVER share this or store it in your history etc.}
}
\value{
a connection object
}
\description{
If you use this method, please delete the FgConnection object when you are done.
Use this method only if connect does not work, e.g. because your keyring does not work or you work in a Jupyter Notebook.
WARNINIG: NEVER store your PAT in your R history or in Source Control.
Anybody who has the PAT can take any action on FASTGenomics you can make.
More Questions? Read our \href{https://github.com/FASTGenomics/fastgenomics-docs/blob/master/doc/api/authorization_guide.md}{in depth authorization Guide}
}
\examples{
connection <- fastgenomicsRclient::connect("https://fastgenomics.org/", "user@example.com", "pat")
}
