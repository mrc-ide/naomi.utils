% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_survey_dhs.R
\name{create_survey_clusters_dhs}
\alias{create_survey_clusters_dhs}
\title{Create survey clusters dataset}
\usage{
create_survey_clusters_dhs(surveys, clear_rdhs_cache = FALSE)
}
\arguments{
\item{surveys}{data.frame of surveys, returned by \code{create_surveys_dhs()}.}
}
\value{
data.frame consisting of survey clusters, survey region id, and
cluster geographic coordinates if available.
}
\description{
Create survey clusters dataset from DHS household recode and
geocluster datasets.
}
\examples{
\dontrun{
surveys <- create_surveys_dhs("MWI")
survey_regions <- create_survey_boundaries_dhs(surveys)
surveys <- surveys_add_dhs_regvar(surveys, survey_regions)

survey_clusters <- create_survey_clusters_dhs(surveys)
}

}
