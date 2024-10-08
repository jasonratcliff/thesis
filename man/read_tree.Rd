% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_data.R
\name{read_tree}
\alias{read_tree}
\title{Read in MrBayes Consensus Tree}
\usage{
read_tree(tree_file)
}
\arguments{
\item{tree_file}{Nexus file of MrBayes results to read in by \code{treeio}.}
}
\value{
Tibble for \code{\link[ggtree:ggtree]{ggtree::ggtree()}} plotting with joined DNA specimen data.
}
\description{
Given an input Nexus file with results from a Mr. Bayes run, split rows of
nodes with identical sample genotypes and join in data from
\link{dna_specimens}.
}
\examples{
list.files(system.file("extdata/MrBayes", package = "thesis"),
  full.names = TRUE, pattern = "rITS-infile.nex.con.tre"
) \%>\%
  read_tree(tree_file = .) \%>\%
  dplyr::select(node, single_label, label)

}
\keyword{internal}
