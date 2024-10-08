% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_beast.R
\name{species_plot}
\alias{species_plot}
\title{Plot *BEAST Species Tree}
\usage{
species_plot(tree_tibble, label_size = 2)
}
\arguments{
\item{tree_tibble}{\code{tbl_tree} object with label vector \code{id_column}.}

\item{label_size}{Numeric vector of length one for label size.}
}
\value{
\code{ggtree} built from *BEAST species tree
}
\description{
Build \link{ggtree} plot from *BEAST maximum credibility clade (\emph{.mcc}) tree file.
Species tip labels are parsed to abbreviate genus and italicize using
base expressions.
}
\details{
On label expressions:
\itemize{
\item https://guangchuangyu.github.io/2018/04/rename-phylogeny-tip-labels-in-treeio/
}
}
\examples{
treeio::read.beast(
  file = system.file("extdata/BEAST/spp-hypothesis-1.mcc",
    package = "thesis"
  )
) \%>\%
  ggtree::fortify() \%>\%
  dplyr::mutate(
    label = gsub("medicinae", "'medicinae'", x = .data$label)
  ) \%>\%
  species_plot(tree_tibble = .)

}
\keyword{internal}
