% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msa_wrappers.R
\name{msa_intervals}
\alias{msa_intervals}
\title{MSA \emph{.tex} Subset Alignment Intervals}
\usage{
msa_intervals(msa_alignment)
}
\arguments{
\item{msa_alignment}{DNAMultipleAlignment object from Biostrings package.}
}
\value{
List of length-2 non-overlapping numeric vectors.
}
\description{
Calculate a list of alignment intervals to subset a mutliple sequence
alignment object. Attempt to avoid Tex capacity issues.
}
\keyword{internal}
