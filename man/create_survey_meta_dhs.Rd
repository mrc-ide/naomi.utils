% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_survey_dhs.R
\name{create_survey_meta_dhs}
\alias{create_survey_meta_dhs}
\title{Create DHS survey meta data table}
\usage{
create_survey_meta_dhs(surveys)
}
\arguments{
\item{surveys}{data.frame of surveys, returned by \code{create_surveys_dhs()}.}
}
\value{
data.frame of survey metadata specification.
}
\description{
Create DHS survey meta data table
}
\examples{
\dontrun{
surveys <- create_surveys_dhs("MWI")
survey_meta <- create_survey_meta_dhs(surveys)
}

}
