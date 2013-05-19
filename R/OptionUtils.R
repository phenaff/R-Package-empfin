##' Trinomial model
##' 
##' Trinomial model for pricing European or American vanilla options.
##' @title Trinomial tree model 
##' @param TypeFlag the option type:
##' \describe{
##' \item{\code{ce}}{European call}
##' \item{\code{pe}}{European put}
##' \item{\code{ca}}{American call}
##' \item{\code{pa}}{American put}
##' }
##' @param S price of underlying asset
##' @param X strike
##' @param Time time to expiry
##' @param r risk-free interest rate
##' @param b cost of carry
##' @param sigma annualized volatility
##' @param n number of steps in tree
##' @return a data structure with the following elements:
##' \describe{
##' \item{\code{param}}{list of input parameters}
##' \item{\code{price}}{price}
##' \item{\code{delta}}{delta}
##' \item{\code{gamma}}{gamma}
##' }
##' @examples
##' res <- CRRTrinomial(TypeFlag="ce", S=100, X=100, Time=1, r=.03, 
##'    b=.03, sigma=.3, n=100) 
##' @export

CRRTrinomial <- function (TypeFlag = c("ce", "pe", "ca", "pa"), S, X, Time, r, 
    b, sigma, n) 
{
    TypeFlag = TypeFlag[1]
    z = NA
    if (TypeFlag == "ce" || TypeFlag == "ca") 
        z = +1
    if (TypeFlag == "pe" || TypeFlag == "pa") 
        z = -1
    if (is.na(z)) 
        stop("TypeFlag misspecified: ce|ca|pe|pa")
    dt = Time/n
    u = exp(sigma * sqrt(2*dt))
    d = 1/u
    dd <- exp(-sigma*sqrt(dt/2))
    pu = ((exp(b * dt/2) - dd)/(1/dd - dd))^2
    pd = (1-sqrt(pu))^2
    pm <- 1-pu-pd
    Df = exp(-r * dt)
    
    # add 1 steps to tree 
    n <- n+1
    # exponent
    iExp <- (1:(2*(n+1)-1))-(n+1)
    OptionValue = z * (S * u^iExp - X)
    OptionValue = (abs(OptionValue) + OptionValue)/2
    if (TypeFlag == "ce" || TypeFlag == "pe") {
        for (j in seq(from = (n), to = 2, by = -1)) 
          for (i in 1:(2*j-1)) 
            OptionValue[i] = (pu*OptionValue[i+2] +
                              pm*OptionValue[i+1] +
                              pd*OptionValue[i]) * Df
    }

    if (TypeFlag == "ca" || TypeFlag == "pa") {
        for (j in seq(from = (n), to = 2, by = -1))
          for (i in 1:(2*j-1)) {
              SS = S * d^(j-1) * u^(i-1)
              exVal =  z * (SS - X)
            OptionValue[i] = (pu*OptionValue[i + 2] +
                              pm*OptionValue[i+1] +
                              pd*OptionValue[i]) * Df
            OptionValue[i] = max(exVal, OptionValue[i])
	  }
    }
    # the middle node is the price
    Sup <- S*u
    Sdown <- S*d
    
    # delta by central difference
    delta <- (OptionValue[3] - OptionValue[1])/(Sup-Sdown)
    du <- (OptionValue[3] - OptionValue[2])/(Sup-S)
    dd <- (OptionValue[2] - OptionValue[1])/(S-Sdown)
    gamma <- (du-dd)/((Sup-Sdown)/2)
 
    param = llist(TypeFlag, S, X, Time, r, b, sigma, n)
    llist(param, price=OptionValue[2], delta, gamma)
}

##' Cox-Ross-Rubinstein binomial model with payoff function
##'
##' @title Cox-Ross-Rubinstein binomial model with payoff function
##'
##' @param TypeFlag e:European exercise, a: American
##' @param PayOff PayOff function: f(underlying value)
##' @param S Spot
##' @param Time Time to maturity
##' @param r interest rate
##' @param b cost of carry
##' @param sigma volatility
##' @param n number of time steps
##' @return PV of option
##' @export

CRRWithPayOff <- function (TypeFlag = c("e", "a"), PayOff, S, Time, r, 
    b, sigma, n) 
{
    TypeFlag = TypeFlag[1]

    dt = Time/n
    u = exp(sigma * sqrt(dt))
    d = 1/u
    p = (exp(b * dt) - d)/(u - d)
    Df = exp(-r * dt)
   
    # underlying asset at step N-1
    ST <- S*(d^(n-1))*cumprod(c(1, rep((u/d), n-1)))
    # at step (n-1), value an European option of maturity dt
    BSTypeFlag <- substr(TypeFlag,1,1)

    OptionValue <- PayOff(ST)

    if (TypeFlag == "e") {
        for (j in seq(from = n - 2, to = 0, by = -1))
          OptionValue <- (p*OptionValue[2:(j+2)] + (1-p)*OptionValue[1:(j+1)])*Df
    }
    
    if (TypeFlag == "a") {
        for (j in seq(from = n - 2, to = 0, by = -1)) {
          ContValue <- (p*OptionValue[2:(j+2)] + (1-p)*OptionValue[1:(j+1)])*Df
          ST <- S*(d^j)*cumprod(c(1, rep((u/d), j)))
          OptionValue <- sapply(1:(j+1), function(i) max(PayOff(ST[i]), ContValue[i]))
        }
    }

    OptionValue[1]
}

##' Trinomial tree plot
##'
##' @title Trinomial tree plot
##'
##' @param TrinomialTreeValues tree values grid
##' @param dx step
##' @param digits format
##' @export

TrinomialTreePlot <- function (TrinomialTreeValues, dx = -0.025, dy = 0.4, cex = 1, 
                               digits = 2,  ...) 
{
  # draw 3 branches originating at node (x,y)
  drawLines <- function(x,y,col=2) {
    xx = c(x,x+1)
    for(k in -1:1) {
      yy = c(y, y+k)
      lines(x=xx,y=yy,col=col)
    }
  }
  Tree = round(TrinomialTreeValues, digits = digits)
  depth = ncol(Tree)
  # frame and coordinates: 
  plot(x = c(1, depth), y = c(-depth+1, depth-1), type = "n", 
       col = 0, yaxt='n', xaxt='n', xlab='step', ylab='', ...)
  axis(1,at=1:depth)
  # tree root
  points(x = 1, y = 0)
  drawLines(1,0)
  text(1 + dx, 0 + dy, deparse(Tree[1, 1]), cex = cex)
  for (i in 1:(depth - 1)) {
    y = seq(from = i, by = -1, length = 2*i + 1)
    x = rep(i, times = length(y)) + 1
    points(x, y, col = 1)
    # place text
    for (j in 1:length(x)) text(x[j] + dx, y[j] + dy, deparse(Tree[j, i + 1]), cex = cex)
    
    if(i<(depth-1)) {
      for (k in 1:length(x)) drawLines(x[k], y[k]) 
    }
  }
  invisible()
}

