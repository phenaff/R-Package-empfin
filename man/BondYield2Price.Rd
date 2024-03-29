% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BondUtils.R
\name{BondYield2Price}
\alias{BondYield2Price}
\title{Bond price from yield}
\usage{
BondYield2Price(bond, dtSettlement, yield)
}
\arguments{
\item{bond}{data structure generated by \code{Bond}}

\item{dtSettlement}{settlement date}

\item{yield}{bond yield}
}
\value{
dirty price
}
\description{
This function computes the bond (dirty) price, given its yield, by the formula
\deqn{
P = \sum_{i=1}^n F_i (1+y)^{-\frac{d_i - d_0}{365}}
}
with:
\describe{
\item{\eqn{F_i}}{Cash flow paid at date \eqn{d_i}}
\item{\eqn{d_i}}{Cash flow date, measured in days}
\item{\eqn{d_0}}{settlement date}
}
}
\examples{
b374 <- Bond('374', myDate('01/01/2000'),
             myDate('04/01/2019'), .0375, 100, 'a')
p <- BondYield2Price(b374, myDate('03/01/2012'), yield=.04)
}
