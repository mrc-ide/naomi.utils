% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_population.R
\name{validate_naomi_population}
\alias{validate_naomi_population}
\title{Validate naomi population dataset}
\usage{
validate_naomi_population(population, areas, area_level)
}
\arguments{
\item{area_level}{area level(s) at which population is supplied}
}
\value{
Invisibly TRUE or raises error.
}
\description{
Validate naomi population dataset
}
\details{
Check that:
\itemize{
\item Column names match schema
\item Population stratification has exactly area_id / sex / age_group for
each year data are supplied
}
}
