\name{nwread}
\alias{nwread}
\title{Read a noweb source file}
\description{
  Read in a noweb source file, parse it, detect any errors, and
  create a \code{noweb} object.
}
\usage{
nwread(file, syntax)
}
\arguments{
  \item{file}{name of the input file to be read}
  \item{syntax}{a list defining the syntax rules for noweb files}
}
\value{
 an object of class \code{noweb}
}
\details{This is an internal function, and
  would rarely if ever by called by a user.
}
\author{Terry Therneau}
\keyword{ programming }
