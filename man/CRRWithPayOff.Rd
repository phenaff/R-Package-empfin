\name{CRRWithPayOff}
\alias{CRRWithPayOff}
\title{Cox-Ross-Rubinstein binomial model with payoff function}
\usage{
  CRRWithPayOff(TypeFlag = c("e", "a"), PayOff, S, Time, r,
    b, sigma, n)
}
\arguments{
  \item{TypeFlag}{e:European exercise, a: American}

  \item{PayOff}{PayOff function: f(underlying value)}

  \item{S}{Spot}

  \item{Time}{Time to maturity}

  \item{r}{interest rate}

  \item{b}{cost of carry}

  \item{sigma}{volatility}

  \item{n}{number of time steps}
}
\value{
  PV of option
}
\description{
  Cox-Ross-Rubinstein binomial model with payoff function
}

