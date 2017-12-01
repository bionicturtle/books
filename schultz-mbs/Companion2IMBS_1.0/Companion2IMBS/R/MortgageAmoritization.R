
  # =============================================
  # Function to amortize a mortgage with Bond Lab
  # =============================================

#' Fixed rate mortgage amortization schedule
#' 
#' @param OriginalBal A numeric value the original loan amount
#' @param NoteRate A numeric value the borrower note rate
#' @param Term A numeric value the term of the loan in months
#' @examples MortgageAmortization(OriginalBal = 200000, NoteRate = 4.5, Term = 360)
#' @importFrom BondLab Mortgage.Monthly.Payment
#' @importFrom BondLab Sched.Prin
#' @importFrom BondLab Remain.Balance 
#' @export
  MortgageAmortization <- function(OriginalBal = numeric(),
                                  NoteRate = numeric(), 
                                  Term = numeric()){
    
    MortgageCashFlow <- array(data = NA, 
                         c(Term, 6))
  
    for(i in 1:Term){
      MortgageCashFlow[i,1] = i
      MortgageCashFlow[i,2] = ifelse((i==1), OriginalBal, MortgageCashFlow[i-1,6])
      MortgageCashFlow[i,3] = Mortgage.Monthly.Payment(orig.bal = OriginalBal, 
                                                       note.rate = NoteRate/100, 
                                                       term.mos = Term)
      MortgageCashFlow[i,4] = NoteRate/1200 * MortgageCashFlow[i,2]
      
      MortgageCashFlow[i,5] = Sched.Prin(balance = OriginalBal, 
                                         note.rate = NoteRate/100,
                                         term.mos = Term,
                                         period = i)
      MortgageCashFlow[i,6] = Remain.Balance(balance = OriginalBal,
                                             note.rate = NoteRate/100,
                                             period = i,
                                             term.mos = Term)
    }
    
    CashFlowTable <- data.frame(Period = MortgageCashFlow[,1],
                                BeginBal = MortgageCashFlow[,2],
                                MonthlyPayment = MortgageCashFlow[,3],
                                ScheduledInterest = MortgageCashFlow[,4],
                                SchedPrin = MortgageCashFlow[,5],
                                EndingBal = MortgageCashFlow[,6])
    print(CashFlowTable)
  }