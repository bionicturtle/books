#Companion to investing in Mortgage Backed Securities

options(digits = 8)
utils::globalVariables(c("day", "month", "year"))
utils::globalVariables(c("x.values", "y.values"))


price.basis <- 100
rate.basis <- 100
tolerance <- .000000001
month.in.year <- 12
trading.days <- 252

