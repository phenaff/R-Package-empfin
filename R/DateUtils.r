## Date utility functions

#' Date creation
#'
#' A shortcut for date creation, locale independent
#' validates day and month ranges
#'
#' @title Date constructor
#' @param dt (string) a date. Legal formats:
#' \describe{
#' \item{\code{ddmmmYYYY}}{ex: 23jun2010}
#' \item{\code{dd-mm-YYYY}}{ex: 23-12-2010}
#' \item{\code{dd/mm/YYYY}}{ex: 23/12/2010}
#' \item{\code{YYYY/mm/dd}}{ex: 2010/12/23}
#' \item{\code{dd.mm.YYYY}}{ex: 23.12.2010}
#' }
#' @return a Date object
#' @examples
#' d1 <- myDate('30-10-2010')
#' d2 <- myDate('30/10/2010')
#' d3 <- myDate('30.10.2010')
#' d4 <- myDate('30oct2010')
#' (d1 == d2) & (d1 == d3) & (d1 == d4)
#' @export

myDate <- function(dt) {
  
  # numeric syntax, year last
  res <- regexpr(.dt.pattern.1, dt)
  if(attr(res, 'match.length') == 10) {
    d <- sub(.dt.pattern.1, '\\1', dt)
    m <- sub(.dt.pattern.1, '\\2', dt)
    y <- sub(.dt.pattern.1, '\\3', dt)    
    return(as.Date(paste(d,'-',m,'-',y,sep=''),'%d-%m-%Y'))
  }
  
  # numeric syntax, year first
  res <- regexpr(.dt.pattern.11, dt)
  if(attr(res, 'match.length') == 10) {
    y <- sub(.dt.pattern.11, '\\1', dt)    
    m <- sub(.dt.pattern.11, '\\2', dt)
    d <- sub(.dt.pattern.11, '\\3', dt)
    return(as.Date(paste(d,'-',m,'-',y,sep=''),'%d-%m-%Y'))
  }
  
  # try month in words
  res <- regexpr(.dt.pattern.2, dt)
  if(attr(res, 'match.length') == 9) {
    d <- sub(.dt.pattern.2, '\\1', dt)
    m <- sub(.dt.pattern.2, '\\2', dt)
    y <- sub(.dt.pattern.2, '\\3', dt)
    # translate month to number to avoid locale issue
    mm <- .valid.months[[tolower(m)]]
    if(is.null(mm)) {
      stop(paste('invalid month: ', mm))
    }  
    return(as.Date(paste(d,'-',mm,'-',y,sep=''),'%d-%m-%Y'))
  }

  stop(paste('invalid date format: ', dt))
}

.valid.months <- list(jan=1, feb=2, fev=2, mar=3, apr=4, avr=4, may=5, mai=5,
                      jun=6, jul=7, aug=8, aou=8, sep=9, oct=10, nov=11, dec=12)

# numeric, ends with year

.dt.pattern.1 <- '^([0]?[1-9]|[1|2][0-9]|[3][0|1])[./-]([0]?[1-9]|[1][0-2])[./-]([0-9]{4}|[0-9]{2})$'
# .dt.pattern.1 <-  '(0?[1-9]|[12][0-9]|3[01])[-/.](0?[1-9]|1[012])[-/.]((19|20)\\d\\d)'

# numeric, starts with year
.dt.pattern.11 <-  '([0-9]{4})[-/.](0?[1-9]|1[012])[-/.](0?[1-9]|1[0-9]|2[0-9]|3[01])'

# month in letters
.dt.pattern.2 <-  '(0?[1-9]|[12][0-9]|3[01])([A-Z,a-z]{3})((19|20)\\d\\d)'


#' timeDate creation
#'
#' A shortcut for date creation, locale independent
#' validates day and month ranges
#'
#' @title dateTime constructor
#' @param dt (string) a date. Legal formats:
#' \describe{
#' \item{ddmmmYYYY}{ex: 23jun2010}
#' \item{dd-mm-YYYY}{ex: 23-12-2010}
#' \item{dd/mm/YYYY}{ex: 23/12/2010}
#' \item{dd.mm.YYYY}{ex: 23.12.2010}
#' }
#' @return a timeDate object
#' @examples
#' d1 <- mytDate('30-10-2010')
#' d2 <- mytDate('30/10/2010')
#' d3 <- mytDate('30.10.2010')
#' d4 <- mytDate('30oct2010')
#' (d1 == d2) & (d1 == d3) & (d1 == d4)
#' @export

mytDate <- function(dt) {
  
  # try numeric syntax
  res <- regexpr(.dt.pattern.1, dt)
  if(attr(res, 'match.length') == 10) {
    d <- sub(.dt.pattern.1, '\\1', dt)
    m <- sub(.dt.pattern.1, '\\2', dt)
    y <- sub(.dt.pattern.1, '\\3', dt)    
    return(as.timeDate(as.Date(paste(d,'-',m,'-',y,sep=''),'%d-%m-%Y')))
  }
  
  # try month in words
  res <- regexpr(.dt.pattern.2, dt)
  if(attr(res, 'match.length') == 9) {
    d <- sub(.dt.pattern.2, '\\1', dt)
    m <- sub(.dt.pattern.2, '\\2', dt)
    y <- sub(.dt.pattern.2, '\\3', dt)
    # translate month to number to avoid locale issue
    mm <- .valid.months[[tolower(m)]]
    if(is.null(mm)) {
      stop(paste('invalid month: ', mm))
    }  
    return(as.timeDate(as.Date(paste(d,'-',mm,'-',y,sep=''),'%d-%m-%Y')))
  }

  stop(paste('invalid date format: ', dt))
}

#' time difference
#'
#' time difference in fraction of years, given 2 timeDate or Date objects
#' @param dtFirst a date or timeDate
#' @param dtLast a date or timeDate
#' @return the time difference in year fraction
#' @export

tDiff <- function(dtFirst, dtLast) {
  as.numeric(as.timeDate(dtLast) - as.timeDate(dtFirst))/365.0
}

#' add days
#'
#' add days to timeDate or Date objects
#' @param dt a Date or timeDate object
#' @param days to be added
#' @return a Date or timeDate object
#' @examples
#' d1 <- myDate('23-10-2010')
#' d2 <- addDays(d1, 1)
#' @export

addDays <- function(dt, days) {
  if(class(dt) == 'timeDate') {
    d <- dt + days*3600*24
  } else {
    d <- dt + days
  }
  
  d
}
