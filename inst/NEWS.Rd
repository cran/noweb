\name{NEWS}
\title{NEWS file for the noweb package}
\section{Changes in version 1.1-4}{
  \itemize{
    \item Fix an issue noted by CRAN. Both notangle and noweave should
    return the name of the file that was created.
}}
\section{Changes in version 1.1-3}{
  \itemize{
    \item A nested construct such as [[ abc[[3]] <- 4]] would crash the
    routine.
}}

\section{Changes in version 1.1-2}{
  \itemize{
    \item Addition of LazyData: true, and a further punctuation change
    in the title field.
}}

\section{Changes in version 1.1-1}{
  \itemize{
    \item Slight change in the Title punctuation, at the request of
    CRAN
}}

\section{Changes in version 1.1}{
  \itemize{
    \item Revise the vignette.  (Inspired by a talk by Donald Knuth at
    userR 2016.)
}}
\section{Changes in version 1.0-3}{
  \itemize{
    \item All code chunks are expected to have a name.  Issue an error
    message if this is not so instead of dying.
}}

\section{Changes in version 1.0-2}{
  \itemize{
    \item Modify the package to work with the non-Sweave vignettes code
    of R 3.0.  This allows the noweb document to be in the vignettes
    directory.
    \item Various updates to the code as I test it.
}}
