#' This function solves for the present value of a lottery payment in Chapter 1
#' 
#' @param payment The annuity payment received by the winner
#' @param interest.rate The discount rate used to determine the present value for example 4.0\% entered as 0.04
#' @param number.period The number of periods example the lottery PV problem is 360 periods
#' @param frequency The frequency payments the lottert PV problem is 12 frequency (monthly)
#' @importFrom BondLab TimeValue
#' @examples LotteryPresentValue(payment = 2777.78, interest.rate = .0530, number.period = 360, frequency = 12)
#' @export
LotteryPresentValue <- function(payment = numeric(),
                           interest.rate = numeric(),
                           number.period = numeric(),
                           frequency = numeric()){
  
  payment * TimeValue(interest.rate = interest.rate, 
                      number.periods = number.period, 
                      frequency = frequency, type = "PVA")
}

#' This function solves for the discount rate of the lottery problem in Chapter 1
#' 
#' @param LumpSumPayment a numeric value the lump sum paid
#' @param payment a numeric value the periodic annuity payment
#' @param number.period a numeric value the number of payments
#' @param frequency a numeric value the frequency of payments
#' @examples  LotteryDiscountRate(LumpSumPayment = 500000, payment = 2777.78, number.period = 360, frequency = 12)
#' @export
LotteryDiscountRate <- function(LumpSumPayment = numeric(),
                                payment = numeric(),
                                number.period = numeric(), 
                                frequency = numeric()){
          #use uniroot to solve for discount rate
          DiscountRate <- uniroot(FindDiscRate, 
                          interval = c(lower = -.2, upper = .2), 
                          tol = tolerance,
                          LumpSumPayment = LumpSumPayment,
                          payment = payment,
                          number.period = number.period,
                          frequency = frequency)$root
          #convert to an annualized value divide by 12
          DiscountRate <- (((1 + (DiscountRate/month.in.year))^month.in.year) - 1) * rate.basis
          
          return(DiscountRate)
}