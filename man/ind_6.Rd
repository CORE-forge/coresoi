% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/06-communication-def-across-crisis.R
\name{ind_6}
\alias{ind_6}
\title{Compute Communication default across the crisis indicator}
\usage{
ind_6(
  data,
  publication_date,
  emergency_name,
  award_col,
  stat_unit,
  test_type,
  cpvs,
  ...
)
}
\arguments{
\item{data}{This argument should be a data frame or tibble containing the data you want to use to calculate the indicator.}

\item{publication_date}{This argument corresponds to the name of the column in data containing the publication date for each notice or report.}

\item{emergency_name}{This argument should be a character string specifying the name of the emergency or event you are analyzing. Examples could include "Coronavirus" or "Terremoto Aquila".}

\item{award_col}{This argument corresponds to the name of the column in data containing the unique identification number for each contract award. This column should contain numeric or character values.}

\item{stat_unit}{This argument should be a character string specifying the statistical unit of measurement or aggregation variable for the indicator. In this indicator contracting authorities are the targets.}

\item{test_type}{This argument should be a character vector specifying the type of hypothesis test belonging to category 1 i.e. see statistical_tests.R)to apply to the data. Available options are "barnard", "fisher", or "z-test".}

\item{cpvs}{character vector of macro-cpv on which data is filtered out. A panel of experts have already chosen which cpvs are most affected by which emergency for you.}

\item{...}{other parameters to pass to \code{generate_indicator_schema} as \code{country_name} if that not Italy, which is default behavior.}
}
\value{
indicator schema as from \code{generate_indicator_schema}
}
\description{
The indicator reveals whether - and to what extent - the duty of contracting authorities to communicate the public procurement procedure activation to the Anticorruption Authority ended with a technical failure.
\subsection{Motivation:}{

The red flag considers at risk contracting authorities \strong{who fail to accomplish their duty} to communicate the public procurement procedure activation to the Anticorruption Authority
}

\subsection{Scoring Rule}{

The output will give $1 - pvalue$, which will then be dichotomised to 1 if statistical test is significant, 0 otherwise.
}

\subsection{Main target unit}{

This indicator targets \strong{contracting authorities}
}
}
\examples{
\dontrun{
if (interactive()) {
  data("mock_data_core")
  ind_6(
    data = mock_data_core,
    publication_date = data_pubblicazione,
    emergency_name = "coronavirus",
    award_col = id_aggiudicazione,
    stat_unit = cf_amministrazione_appaltante,
    test_type = "fisher"
  )
}
}
}
\seealso{
\code{\link[lubridate]{ymd}}
\code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{arrange}}
\code{\link[forcats]{as_factor}}
\code{\link[stringr]{str_sub}}
}
