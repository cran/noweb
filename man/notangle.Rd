\name{notangle}
\alias{notangle}
\title{Extract code from a noweb object}
\description{
 Extract a named bit of code from the noweb source of R functions
}
\usage{
notangle(file, target = "*", out, syntax = nowebSyntax, \ldots)
}
\arguments{
  \item{file}{an input file containing noweb code, or alternately
    an object of class \code{noweb} as 
    created by the \code{nwread} function.
}
\item{target}{the target code chunk to extract.  The parent .Rnw file
  will contain \code{<<chunk-name>>} directives.
  A standard approach is to use * as the name of a chunk that will
  extract all the code fragments.
}
\item{out}{a file name on which to write the result. By default this
  will be the name of the target with ".R" added, or if the target
  is "*" it will be the name of the input file with it's final
  suffix changed to ".R".
  A value of \code{out=NULL} can be used to suppress file creation.
}
\item{syntax}{defines the syntax to detect code chunks.  This would
  rarely if ever be changed by a user.}
\item{\ldots}{extra arguments; unused.  (Compatability with Sweave).}
}
\value{
  A character vector containing the code, with the invisible attribute
  set to prevent automatic printing.
}
\references{N Ramsay. Literate programming simplified. IEEE Software
  11:97-105, 1994.
}
\author{Terry Therneau}
\seealso{\code{\link{noweave}}}
\examples{
\dontrun{
 # Extract the nwread function
 notangle("noweb.Rnw", "nwread")
}}
\keyword{ programming }

