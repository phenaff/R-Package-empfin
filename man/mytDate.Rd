\name{mytDate}
\alias{mytDate}
\title{dateTime constructor}
\usage{
  mytDate(dt)
}
\arguments{
  \item{dt}{(string) a date. Legal formats: \describe{
  \item{ddmmmYYYY}{ex: 23jun2010} \item{dd-mm-YYYY}{ex:
  23-12-2010} \item{dd/mm/YYYY}{ex: 23/12/2010}
  \item{dd.mm.YYYY}{ex: 23.12.2010} }}
}
\value{
  a timeDate object
}
\description{
  timeDate creation
}
\details{
  A shortcut for date creation, locale independent
  validates day and month ranges
}
\examples{
d1 <- mytDate('30-10-2010')
d2 <- mytDate('30/10/2010')
d3 <- mytDate('30.10.2010')
d4 <- mytDate('30oct2010')
(d1 == d2) & (d1 == d3) & (d1 == d4)
}

