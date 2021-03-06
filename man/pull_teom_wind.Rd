% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_functions.R
\name{pull_teom_wind}
\alias{pull_teom_wind}
\title{Pull wind data from portable TEOM stations from database.}
\usage{
pull_teom_wind(date1, date2)
}
\arguments{
\item{date1, }{date2 Text string. Date range for which to pull data.}
}
\value{
Data frame.
}
\description{
Pull wind speed and direction from teoms.teom_summary_data in AirSci 
PostgreSQL database. This function pulls only for stations required for the 
TwB2 paired TEOM reporting.
*Note: PM10 data from this is the analog averaged data transitted via 
LoggerNet. The PM10 data from this table should **not** be used for 
reporting.*
}
\examples{
pull_teom_wind("2016-02-01", "2016-03-01")
}

