% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/easy-ech_mercado_trabajo.R
\name{estimate_labor_indicators.}
\alias{estimate_labor_indicators.}
\title{Estimate labor market indicators based on the ECH}
\usage{
estimate_labor_indicators.(
  .data,
  .ech,
  .by = NULL,
  .recipe = "labor",
  .load_recipe = TRUE
)
}
\arguments{
\item{.data}{Tibble with the survey}

\item{.ech}{Survey year}

\item{.by}{a variable to agroup the data}

\item{.recipe}{Recipe name}
}
\value{
Returns a tibble with the created variables
}
\description{
Estimate labor market indicators based on the ECH
}
\examples{


devtools::load_all()
load_base.(
  .dir = here::here(
    "P_2019_Terceros.sav"
  )
) \%>\%
  estimate_labor_indicators.(
    .ech = "2019",
    .by = c("nomdpto", "e26")
  )
}
\keyword{easy}
\keyword{ech}
