\name{noweb.sty}
\docType{data}
\alias{noweb.sty}
\title{Style file for noweb documents}
\description{Style file for noweb documents}
\usage{noweb.sty}
\format{vector of character strings}
\details{
  Latex files created by the noweave command will contain the line
  \code{usepackage\{noweb\}}, which in turn will try to load the file
  \code{noweb.sty}.  A copy of this file needs to be either in the local
  directory or on the search path used by Latex.
}
\examples{
#Write a copy of the file in the local directory
data(noweb)
cat(noweb.sty, sep='\n', file="noweb.sty")
}
\keyword{datasets}
