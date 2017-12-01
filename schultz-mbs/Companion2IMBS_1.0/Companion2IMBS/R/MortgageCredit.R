#' Function for sizing mortgage credit enhancement
#' 
#' This function sizes mortgage credit enhancement levels.  The funtion illustrates how
#' one may create a new analytic function by calling individial BondLab functions.  In this case
#' we create a new function CreditEnhancement
#' @param bond.id a character string the bond id
#' @param original.balance a numeric value the original balance
#' @param trade.date a character string the trade date
#' @param settlement.date a character string the settlement date
#' @param sigma a numeric value the **annualized** volatility assumption used for 
#' the CIR interest rate model
#' @param HPA a numeric value the long run expected home price appreciation for example 
#' 2.5\% is entered as 0.025
#' @param HPV the annualized volatility of home prices for example 1.5\% is entered as 0.015 
#' @param paths a numeric value the number of paths
#' @importFrom BondLab BondBasisConversion
#' @importFrom BondLab MortgageCashFlow
#' @importFrom BondLab PrepaymentAssumption
#' @importFrom BondLab ModelTune
#' @importFrom BondLab CalibrateCIR
#' @importFrom BondLab CIRSim
#' @importFrom BondLab CIRBondPrice
#' @importFrom BondLab HPISim
#' @importFrom BondLab MtgRate
#' @importFrom BondLab TermStructure
#' @importFrom BondLab MBS
#' @import ggplot2
#' @import reshape2
#' @examples CreditEnhancement(bond.id = bondlabMBS4, original.balance = 100000, 
#' trade.date = "01-10-2013", settlement.date = "01-13-2013", sigma = .2, HPA = .025, 
#' HPV = .015, paths = 100)
#' @export
CreditEnhancement<- function(bond.id = character,
                            trade.date = character,     
                            settlement.date = character,
                            original.balance = numeric(),
                            sigma = numeric(),
                            HPA = numeric(),
                            HPV = numeric(),
                            paths = numeric()){
  Period <- NULL
  Value <- NULL
  Variable <- NULL
  ..density.. <- NULL

Palette <- c("#E69F00", "#56B4E9", "#CC79A7", "#009E73", "#F0E442","#D55E00", "#0072B2", "#000000")
cbbPalette <- rep(Palette, length.out = paths)

#Call Rates Data
rates.data = Rates(trade.date = trade.date)
short.rate = as.numeric(rates.data[1,2])/rate.basis

#Call Mortgage Rate
MortgageRate = MtgRate()

#Call MBS data
bond.id = MBS(MBS.id = "bondlabMBS4")
Burnout = bond.id@Burnout

issue.date = as.Date(bond.id@IssueDate, "%m-%d-%Y")
start.date = as.Date(bond.id@DatedDate, "%m-%d-%Y")
end.date = as.Date(bond.id@Maturity, "%m-%d-%Y")
lastpmt.date = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
nextpmt.date = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
coupon = bond.id@Coupon
frequency = bond.id@Frequency
delay = bond.id@PaymentDelay
factor = bond.id@MBSFactor
settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")

ModelTune = ModelTune(bond.id = bond.id)

#Override Voluntary repayment
ModelTune@TurnoverRate <- 0
ModelTune@Incentive.Fast.theta.1 <- 0
ModelTune@Incentive.Fast.theta.2 <- 0
ModelTune@Incentive.Slow.theta.1 <- 0
ModelTune@Incentive.Slow.theta.2 <- 0

#Calculate the number of cashflows that will be paid from settlement date 
#to the last pmt date (used end date as next pmdt date for this)
ncashflows = BondBasisConversion(issue.date = issue.date, 
                                 start.date = start.date, 
                                 end.date = end.date, 
                                 settlement.date = settlement.date,
                                 lastpmt.date = lastpmt.date, 
                                 nextpmt.date = end.date) 

#Build a vector of dates for the payment schedule
#first get the pmtdate interval
pmtdate.interval = month.in.year/frequency

#Compute the payment dates 
pmtdate = as.Date(c(if(settlement.date == issue.date) 
{seq(start.date, end.date, by = paste(pmtdate.interval, "months"))} 
else 
{seq(nextpmt.date, end.date, by = paste(pmtdate.interval, "months"))}), "%m-%d-%Y") + delay


#Build the time period vector (n) for discounting the cashflows nextpmt date 
#is vector of payment dates to n for each period
time.period = BondBasisConversion(issue.date = issue.date, 
                                  start.date = start.date, 
                                  end.date = end.date, 
                                  settlement.date = settlement.date,
                                  lastpmt.date = lastpmt.date, 
                                  nextpmt.date = pmtdate)

#step4 Count the number of cashflows 
#num.periods is the total number of cashflows to be received
#num.period is the period in which the cashflow is received
num.periods = length(time.period)
num.period = seq(1:num.periods)


#Calibrate the CIR Model
Market.Fit <- CalibrateCIR(trade.date = trade.date, sigma = sigma)
kappa  = Market.Fit$p1
lambda = Market.Fit$p2
theta  = Market.Fit$p3

#Call CIR Model 
#The CIR Model will pass the rates data to both the prepayment model
#and the home price simulation model
set.seed(300)
InterestRateSimulation <- CIRSim(shortrate = short.rate, 
                                 kappa = kappa, 
                                 theta = theta, 
                                 T = 30, 
                                 step = (1/12), 
                                 sigma = sigma, 
                                 N = paths)

#Create the matrix of simulated Home Prices
HomePriceSimulation <- NULL 
for(i in 1:ncol(InterestRateSimulation)){
  
  HomePrice = HPISim(shortrate = InterestRateSimulation[2,i],
                     LongTermGrowth = HPA,
                     sigma = HPV,
                     T = 30,
                     step = (1/12),
                     N = 1)
  HomePriceSimulation = cbind(HomePriceSimulation, HomePrice)} 

#================ Loop through the home price simulations ===============
DefaultVector = num.period
LossAmount = num.period
CumLoss = num.period
AvgLife = NULL

for(j in 1:ncol(InterestRateSimulation)) {
  cum.rate = cumprod(1 + InterestRateSimulation[2:361,j])
  spot.rate = (((cum.rate^(1/num.period)) ^ (1/month.in.year))-1)
  
  TermStructure <- new("TermStructure",
                       tradedate = "01-10-2013",
                       period = num.period,
                       date = as.character(pmtdate),
                       spotrate = spot.rate,
                       forwardrate = InterestRateSimulation[2:361,j],
                       TwoYearFwd = CIRBondPrice(shortrate = as.numeric(InterestRateSimulation[2:361, j]), 
                                    kappa = kappa, 
                                    lambda = lambda, 
                                    theta =  theta, 
                                    sigma = sigma, 
                                    T = 2, 
                                    step = 0, 
                                    result = "y") * 100,
                       TenYearFwd = CIRBondPrice(shortrate = as.numeric(InterestRateSimulation[2:361, j]), 
                                    kappa = kappa, 
                                    lambda = lambda, 
                                    theta = theta, 
                                    sigma = sigma, 
                                    T = 10, 
                                    step = 0, 
                                    result = "y") * 100)
  
  ProjectedDefault <- PrepaymentAssumption(bond.id = bond.id, 
                                           TermStructure = TermStructure, 
                                           MortgageRate = MortgageRate, 
                                           PrepaymentAssumption = "MODEL", 
                                           ModelTune = ModelTune, 
                                           Burnout = Burnout,
                                           CPR = 0,
                                           HomePrice = HomePriceSimulation[2:361,j])
  
  MortgageCashFlow <- MortgageCashFlow(bond.id = bond.id,
                                       original.bal = original.balance,
                                       settlement.date = settlement.date,
                                       price = 100,
                                       PrepaymentAssumption = ProjectedDefault)
  
  
  DefaultVector = cbind(DefaultVector, ProjectedDefault@MDR)
  LossAmount = cbind(LossAmount, MortgageCashFlow@DefaultedPrin)
  CumLoss <- cbind(CumLoss, cumsum(MortgageCashFlow@DefaultedPrin))
  AvgLife <- append(AvgLife, MortgageCashFlow@WAL)
  
    }

  CumLossPct <- data.frame(CumLoss[,2:paths]/original.balance)

  CumLossPct<- cbind(num.period, CumLossPct)
  CumLossPct <- melt(CumLossPct, id = "num.period")
  colnames(CumLossPct) <- c("Period", "Variable", "Value" )

  CumLossGraph <- ggplot(CumLossPct, aes(x=Period, y = Value * 100, colour = Variable, group = Variable)) +
  geom_line(size = .5, alpha = 1) +
  scale_colour_manual(values = cbbPalette) +
  labs(colour = "Legend", linetype = "Legend", x = "Time Period (mos.)", y = "Cumulative Principal Default") +
  theme_minimal() +
  #scale_y_continuous(limits = c(0,60), breaks = seq(0, 60, 5)) +
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, 36)) +
  theme(panel.grid.major = element_line(size = .50, color = "grey")) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 20)) + 
  theme(legend.position = "none")

# ============================================

  CumLoss <- data.frame(CumLoss)
  CumLossDist  <- ggplot(subset(CumLossPct, CumLossPct$Period == 360), aes(x= Value * .35 * 100)) +
  
  geom_histogram(aes(y=..density..),
                 binwidth = .125,
                 colour = "lightgrey",
                 fill = "#0072B2") +
  geom_density(alpha = .5, fill = "#0072B2", colour = "lightgrey") +
  theme_minimal() +
  ylab("Density of Loss Function")+
  xlab("Cumulative Loss") +
  theme(panel.grid.major = element_line(size = .50, color = "grey")) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 20)) + 
  theme(legend.position = "none")

  layout <- grid.layout(nrow = 3, ncol = 2,
                      widths = unit(c(2,2), c("null", "null")),
                      heights = unit(c(4, .2 , 2), c("lines", "null", "null")))

  vplayout <- function(...){
  grid.newpage()
  pushViewport(viewport(layout = layout))}

  subplot <- function(x, y) {viewport(layout.pos.row = x,
                                    layout.pos.col = y)}
  mmplot <- function(a, b) {
  vplayout()
  grid.text("Bond Lab Credit Analysis", x = .1, y = .7, just = "left", vp = subplot(1,1))
  print(a, vp = subplot(3, 1))
  print(b, vp = subplot(3, 2))
}

mmplot(CumLossGraph, CumLossDist)

on.exit(closeAllConnections())

  }