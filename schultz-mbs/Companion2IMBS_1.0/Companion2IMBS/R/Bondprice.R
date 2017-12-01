#' bond.price illustrates the basic compution of the price of a bond
#'  
#' Computation of price as the present value of the coupon annuity payment and zero coupon price of the principal payment at maturity
#' This function is taken directly from BondLab and is copyright BondLab Technologies, Inc.
#' @param yield.to.maturity numeric value the yield to maturity of the bond for example 4.0\% entered as 0.04
#' @param coupon numeric value the coupon of the bond for example 4.0\% entered as  0.04
#' @param coupon.frequency a numeric value the number of coupon payments for example semi-annual entered as 2
#' @param years.mat a numeric value remaining years to maturity.  This is a simple function, remaining years must equal frequency
#' @param face.value a numeric value the notional or par amount of the bond
#' @examples bond.price(yield.to.maturity = .04, coupon = .04, coupon.frequency = 2, 
#' years.mat = 10, face.value = 100) 
#' @export
bond.price <- function(yield.to.maturity, 
                     coupon,
                     coupon.frequency, 
                     years.mat, 
                     face.value){
  i = yield.to.maturity/coupon.frequency
  c = coupon/coupon.frequency
  n = years.mat * coupon.frequency
  fv = face.value
  
  bondvalue = ((((1-(1/(1+i)^n))/i) * (fv * c)) +
                 (1/(1+i)^n * fv))
  
  return((bondvalue/face.value) * price.basis)
  }

#' bond.irr is an intermediate function to compute bond yield to maturity
#' 
#' @param yield.to.maturity a numeric value the yield to maturity
#' @param coupon a numeric value the bond coupon
#' @param coupon.frequency a numeric value the frequency of coupon payments
#' @param years.mat a numeric value the years to maturity
#' @param price a numeric value the price of the bond
#' @param face.value a numeric value the face value of the bond
#' @export
bond.irr <- function(yield.to.maturity, 
                     coupon,
                     coupon.frequency, 
                     years.mat,
                     price = price,
                     face.value){

  price = price / price.basis
  outlay = price * face.value
  
  bondprice = bond.price(yield.to.maturity = yield.to.maturity,
                         coupon = coupon,
                         coupon.frequency = coupon.frequency,
                         years.mat = years.mat,
                         face.value = face.value)
  
  proceeds <- (bondprice/price.basis) * face.value
  outlay - proceeds}

#' YTM calculates the bond yield to maturity
#' 
#' The function calls uniroot and bond.irr
#' @param coupon a numeric value the coupon rate example 5.0\% is entered as 0.05
#' @param coupon.frequency a numeric value the annual payment frequency
#' @param years.mat a numeric value the years to maturity
#' @param price a numeric value the price
#' @export
YieldToMaturity <- function(coupon = numeric(),
                            coupon.frequency = numeric(),
                            years.mat = numeric(),
                            price = numeric()){
  
                YTM <- uniroot(bond.irr, 
                interval = c(-.2, .2),
                tol = tolerance,
                coupon = coupon, 
                coupon.frequency = coupon.frequency, 
                years.mat = years.mat, 
                price = price, 
                face.value = 1000)$root
                
                YTM <- YTM * 100
                
                return(YTM)}

#' Bond duration is a wrapper around Bond.Cash.Flow
#' 
#' The function returns bond duration as calculated from
#' the bond cash flow table
#' @param issue.date a character string the issue date
#' @param start.date a character string the start date
#' @param end.date a character string the end date
#' @param coupon a numeric value the coupon 4.0\% is entered as 0.04
#' @param frequency a numeric value the number of payments annually
#' @param price a numeric value the bond price
#' @examples BondDuration(issue.date = "01-10-2013", 
#' start.date = "01-15-2013", end.date = "01-15-2023", coupon = .05, frequency = 2, price = 100)
#' @export
BondDuration <- function(issue.date = character,
                         start.date = character,
                         end.date = character,
                         coupon = numeric(),
                         frequency = numeric(),
                         price = numeric()){
  
               BondCashFlow <- Bond.Cash.Flow(issue.date = issue.date,
                               start.date = start.date,
                               end.date = end.date,
                               coupon = coupon,
                               principal = 1000,
                               frequency = frequency,
                               price = price)

    Duration = sum(BondCashFlow$pv.period) /price.basis
    Convexity = (1/2) * (sum(BondCashFlow$cf.cvx.period) /price.basis)
    
    cat(paste("Duration", round(Duration, digits = 2)), sep = ":")

} 