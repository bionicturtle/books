## ---- echo= FALSE, verbose = FALSE, results = 'hide', warning=FALSE, message=FALSE----
require(Companion2IMBS)

## ---- echo = TRUE--------------------------------------------------------
PresentValue(interest.rate = .05, number.period = 3, frequency = 1)

## ---- echo = TRUE--------------------------------------------------------
FutureValue(interest.rate = .05, number.period = 3, frequency = 1)

## ---- echo = TRUE--------------------------------------------------------
LotteryPresentValue(payment = 2777.78, interest.rate = .0530, 
                    number.period = 360, frequency = 12)

## ---- echo = TRUE--------------------------------------------------------
LotteryDiscountRate(LumpSumPayment = 500000, payment = 2777.78, 
                    number.period = 360, frequency = 12)

## ---- fig.height= 5, fig.width= 7, echo=TRUE, verbose = FALSE, warning= FALSE----
PlotTermStructure(trade.date = "01-10-2013", method = "ns")

## ---- echo= TRUE, verbose = FALSE, warning=FALSE-------------------------
signif(ReturnAnalysis(), digits = 4)

## ----echo = TRUE, verbose = FALSE, warning = FALSE-----------------------
ReturnMatrix <- ReturnAnalysis()
AnnualizedReturn(BeginValue = 1000,
                 EndValue = sum(ReturnMatrix[1:10, 10, 1]),
                 TimeHorizon = 5,
                 Frequency = 2)

