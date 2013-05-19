\name{myDate}
\alias{myDate}
\title{Date constructor}
\usage{
  myDate(dt)
}
\arguments{
  \item{dt}{(string) a date. Legal formats: \describe{
  \item{\code{ddmmmYYYY}}{ex: 23jun2010}
  \item{\code{dd-mm-YYYY}}{ex: 23-12-2010}
  \item{\code{dd/mm/YYYY}}{ex: 23/12/2010}
  \item{\code{YYYY/mm/dd}}{ex: 2010/12/23}
  \item{\code{dd.mm.YYYY}}{ex: 23.12.2010} }}
}
\value{
  a Date object
}
\description{
  Date creation
}
\details{
  A shortcut for date creation, locale independent
  validates day and month ranges
}
\examples{
d1 <- myDate('30-10-2010')
d2 <- myDate('30/10/2010')
d3 <- myDate('30.10.2010')
d4 <- myDate('30oct2010')
(d1 == d2) & (d1 == d3) & (d1 == d4)
}

