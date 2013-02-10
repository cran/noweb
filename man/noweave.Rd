\name{noweave}
\alias{noweave}
\title{Extract documentation from a noweb object}
\description{
 Write a TeX file from a noweb source file.
}
\usage{
noweave(file, out, indent=1, syntax=nowebSyntax, ...)
}
\arguments{
  \item{file}{an input file containing noweb code, or alternately
    an object of class \code{noweb} as 
    created by the \code{nwread} function.
  }
  \item{out}{a file name on which to write the result. By default this
  will be the name of the input file with it's final
  suffix changed to ".tex".
  }
  \item{indent}{the amount to indent code chunks}
\item{syntax}{a list defining the syntax rules for noweb}
\item{\dots}{unused argments needed to match Sweave usage}
}
\details{
  R code written using noweb is a mixture of Latex documentation and
  chunks of R code.  The \code{noweave} command formats these into a TeX
  document which is designed to explain the code to a human.
  The \code{notangle} command writes out R files for the computer's use.
  }
\author{Terry Therneau}
\seealso{\code{\link{nwread}}, \code{\link{notangle}}}
\examples{
\dontrun{
 # Create the TeX file of documentation for the noweb library
 noweave("noweb.Rnw"}
}
\keyword{ programming }


