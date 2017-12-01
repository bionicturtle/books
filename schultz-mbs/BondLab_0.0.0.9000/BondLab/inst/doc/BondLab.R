## ---- echo = FALSE, results = 'hide', warning=FALSE, message=FALSE-------
require(BondLab)

## ---- echo = TRUE--------------------------------------------------------
CPR.To.SMM(CPR = .06)

## ---- echo = TRUE--------------------------------------------------------
SMM.To.CPR(SMM = .005)

## ---- echo= TRUE---------------------------------------------------------
SMMVector.To.CPR(SMM = vector(), num.period = vector())

## ---- echo=TRUE, fig.width= 5, fig.height= 4, fig.align='center'---------
PPC <- PPC.Ramp(season.period = 30, begin.cpr = .02, end.cpr = .06, period = seq(1, 60, 1))
plot(PPC * 100, type ="l", xlab = "Loan Age", ylab = "CPR", col = "blue", bty = "n")

## ---- echo=TRUE----------------------------------------------------------
Mortgage.Monthly.Payment(orig.bal = 100000, note.rate = .04, term.mos = 360)

## ---- echo=TRUE----------------------------------------------------------
Sched.Prin(balance = 100000, note.rate = .045, term.mos = 360, period = 2)

