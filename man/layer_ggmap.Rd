% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprec-maps.R
\name{layer_ggmap}
\alias{layer_ggmap}
\title{Layer ggmap base for specimen plot.}
\usage{
layer_ggmap(
  specimen_tbl,
  zoom_lvl = 7,
  gg_map_type = c("terrain", "satellite", "roadmap", "hybrid"),
  gg_longitude = NULL,
  gg_latitude = NULL,
  ...
)
}
\arguments{
\item{specimen_tbl}{Tibble of herbarium specimens subset.}

\item{zoom_lvl}{Integer passed to \code{ggmap::get_map()} argument \code{zoom}.}

\item{gg_map_type}{Type of map to return from \code{ggmap::get_map()} function
call. One of: "terrain", "satellite", "roadmap" or "hybrid".}

\item{gg_longitude}{Numeric vector of length one to optionally center call
of \code{get_map()} longitude coordinates.}

\item{gg_latitude}{Numeric vector of length one to optionally center call
of \code{get_map()} latitude coordinates.}

\item{...}{Forwarded arguments from \link{build_map}}
}
\value{
Base ggplot layer from \link{ggmap} raster object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
Build base ggplot layer for specimen mapping over satellite or terrain
rasters images. Access to the static maps requires API key registration. To
check if the key has been set in the users \code{.Renviron} file, see
\code{ggmap::has_google_key()} \cr
For more information, see the \code{README} at: https://github.com/dkahle/ggmap
}
\examples{
ggmap_ggplot <- layer_ggmap(
  specimen_tbl = thesis::spp_co_front_range,
  gg_map_type = "satellite", zoom_lvl = 8
)
ggmap_ggplot +
  layer_borders(spl_extent = spl_bbox(ggmap_ggplot)) +
  layer_specimens(
    specimen_tbl = spp_co_front_range, shape_aes = TRUE,
    id_column = "Taxon_a_posteriori"
  ) +
  layer_themes(
    specimen_tbl = spp_co_front_range,
    id_column = "Taxon_a_posteriori",
    legend_title = "Reviewed Annotations"
  )

}
\references{
D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R
Journal, 5(1), 144-161. URL
http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
}
\keyword{internal}
