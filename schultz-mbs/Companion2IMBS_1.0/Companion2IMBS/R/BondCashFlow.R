#' Bond Cash Flow
#' 
#' Cash flow analysis of a standard non-callable bond
#' This function is taken from BondLab and is copyright BondLab Technologies, Inc.
#' see BondLab documentation BondAnalytics
#' @param issue.date A character string the issue date
#' @param start.date A character string the start or dated date
#' @param end.date A character string the bond's maturity
#' @param coupon A numeric the coupon of the bond
#' @param principal A numeric value the principal of the bond
#' @param frequency A numeric value the payment frequency
#' @param price A numeric value the price of the bond
#' @importFrom lubridate day
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @examples Bond.Cash.Flow(issue.date = "01-01-2013", start.date = "01-15-2013", 
#' end.date = "01-15-2023", coupon = .05, principal =1000, frequency = 2, price = 100)
#' @export
Bond.Cash.Flow <-function(issue.date = "character", 
                          start.date = "character", 
                          end.date = "character", 
                          coupon = numeric(), 
                          principal = numeric(), 
                          frequency = numeric(), 
                          price = numeric()){
  issue.date <- as.Date(c(issue.date), "%m-%d-%Y")
  start.date <- as.Date(c(start.date), "%m-%d-%Y")
  end.date <- as.Date(c(end.date), "%m-%d-%Y")
  price = price/price.basis
  
  # 30/360 day count calculation 
  d1 = day(issue.date)
  m1 = month(issue.date)
  y1 = year(issue.date)
  d2 = day(end.date)
  m2 = month(end.date)
  y2 = year(end.date)
  
  diff = (max(0, 30 - d1) + min(30, d2) + 360*(y2-y1) + 30*(m2-m1-1))/360
  ncashflows = diff * frequency
  cf.period = seq(1:ncashflows)
  pmtdate = seq(start.date, end.date, by = "6 months")
  
  time.period = (cf.period * 6)/12
  
  couponincome = rep(coupon/frequency * principal, ncashflows)
  principalincome = rep(0,ncashflows)
  principalincome[ncashflows] = principal
  cashflow = couponincome + principalincome
  
  #===============================================================================================
  # Calculate Yield to maturity
  irr <- function(rate, time.period, cashflow, principal, price){
    pv = cashflow * 1/(1+rate)^time.period
    proceeds = principal * price
    sum(pv) - proceeds
  }
  
  ytm = uniroot(irr, interval = c(lower = -.20, upper = .20), tol = tolerance, time.period = time.period, 
                cashflow = cashflow, principal = principal, price = price)$root
  
  #===============================================================================================
  # The following calculates the duration and convexity of a standard non-callable bond
  #PV Calculations
  ytm.vec = c(rep(ytm,ncashflows))
  pv.factor = 1/(1+ytm.vec)^time.period
  pv.cashflow = cashflow*pv.factor
  
  #Duration
  pv.price = pv.cashflow /(principal * (price/price.basis))
  pv.period = pv.price * time.period
  
  (sum(pv.period) / (price * price.basis)/( 1+ (ytm/frequency)))
  
  
  #Convexity
  cvx.time = time.period*(time.period + 1)
  cf.cvx = (cashflow/(1+ytm)^(time.period + 2))/(principal * (price/price.basis))
  cf.cvx.period = cf.cvx * cvx.time
  
  cashflow.table <- data.frame(Period = cf.period, Time = time.period, Cashflow = cashflow, PVFactor = pv.factor,
                               PV = pv.cashflow, PV.Price = pv.price, pv.period = pv.period, 
                               cvx.time = cvx.time, cf.cvx = cf.cvx, cf.cvx.period = cf.cvx.period )
  
  
  convexity <-
    (sum(pv.period) / (price * price.basis))/( 1+ (ytm/frequency))
  .5 * ((sum(cf.cvx.period)/(price * price.basis)))
  #====================================================================================================
  return(data.frame(cashflow.table))
}
