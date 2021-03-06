% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_survey_dhs.R
\name{validate_survey_region_areas}
\alias{validate_survey_region_areas}
\title{Validation of mapping to survey region areas}
\usage{
validate_survey_region_areas(
  survey_region_areas,
  survey_region_boundaries,
  warn = FALSE
)
}
\arguments{
\item{survey_region_areas}{Allocation of areas to survey regions, returned by
\code{\link[=allocate_areas_survey_regions]{allocate_areas_survey_regions()}}.}

\item{survey_region_boundaries}{survey_region_boundaries dataset created by
\code{create_survey_boundaries_dhs()}.}

\item{warn}{Raise a warning instead of an error (default \code{FALSE})}
}
\value{
invisibly TRUE or raises an error.
}
\description{
Validation of mapping to survey region areas
}
\details{
Conducts checks on \code{survey_region_areas}:
\itemize{
\item All areas have been mapped to a survey region in each survey.
\item All survey regions contain at least one area. Otherwise no clusters could
have come from that survey region.
}

Passing these checks does not confirm the mapping is accurate, but these checks
will flag inconsistencies that need cleaning.
}
