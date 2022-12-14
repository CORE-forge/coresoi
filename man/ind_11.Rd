% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/11-dist-betw-awd-val-sums-paid.R
\name{ind_11}
\alias{ind_11}
\title{Compute Distance between award value and sums paid indicator}
\usage{
ind_11(
  data,
  award_value,
  sums_paid,
  stat_unit,
  outbreak_starting_date = lubridate::ymd("2017-06-30"),
  publication_date
)
}
\arguments{
\item{data}{data to be passed, expects tibble}

\item{award_value}{The date when the tender was awarded}

\item{sums_paid}{The amount paid by the C.A.}

\item{stat_unit}{the statistical unit of measurement (can be a vector of grouping variables), i.e. variable to group by}

\item{outbreak_starting_date}{The date when the emergency officially started, Default: lubridate::ymd("2017-06-30")}

\item{publication_date}{The date when the tender was published}
}
\value{
indicator schema as from \code{generate_indicator_schema}
}
\description{
The difference between the sum foreseen in the contract and the actual payment by the C.A. (ita SA stazione Appaltante)
}
\details{
DETAILS
}
\examples{
\dontrun{
if (interactive()) {
  ind_11(
    data = mock_data_core, publication_date = data_pubblicazione,
    award_value = importo_aggiudicazione, sums_paid = importo_lotto,
    cf_amministrazione_appaltante,
    outbreak_starting_date = lubridate::ymd("2017-06-30")
  )
}
}
}
\seealso{
\code{\link[lubridate]{ymd}}
\code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}
\code{\link[forcats]{as_factor}}
}
