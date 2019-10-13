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

  This data set is a copy of the file; create a disk copy for Latex by
  using \code{write(noweb.sty, file="noweb.sty")}.
}
\keyword{datasets}
