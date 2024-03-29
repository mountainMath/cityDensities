% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/city_densities.R
\name{plot_density_facet}
\alias{plot_density_facet}
\title{Plot fact graph with city maps and density patterns}
\usage{
plot_density_facet(
  cities,
  bks = c(4, 10, 25, 50, 100, 200, 500, 1000),
  radius_km = 40,
  years = c("1975", "1990", "2000", "2015"),
  remove_lowest = TRUE,
  lowest_color = NULL
)
}
\arguments{
\item{cities}{list of cities of class `sf` point geometries and `name` field}

\item{bks}{break points for poopulation densities}

\item{years}{list of years, valid values are `1975`, `1990`, `2000`, `2015`}

\item{remove_lowest}{logical, remove population below the lowest density breakpoint}

\item{lowest_color}{colour for lowest poulation density bracket if not removed}

\item{max_radius_km}{radius to compute the densities}
}
\value{
a ggplot2 object
}
\description{
Plot fact graph with city maps and density patterns
}
