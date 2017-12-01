  #' Function plots a mortgage pass-thorugh security cash flow
  #' @param bond.id a character string the bond id
  #' @param trade.date a character string the trade date
  #' @param settlement.date a character string the settlement date
  #' @param begin.cpr a numeric value the first month CPR assumption
  #' @param end.cpr a numeric value end CPR of the seasoning ramp
  #' @param seasoning.period a numeric value the length of the seasoning ramp
  #' @importFrom BondLab PassThroughAnalytics
  #' @import ggplot2
  #' @import reshape2
  #' @examples PassThroughCashFlow(bond.id = "bondlabMBS4", trade.date = "01-10-2013", 
  #' settlement.date = "01-13-2013", begin.cpr = .002, end.cpr = .06, seasoning.period = 30)
  #' @export
  PassThroughCashFlow <- function(bond.id = character,
                                 trade.date = character,
                                 settlement.date = character,
                                 begin.cpr = numeric(),
                                 end.cpr = numeric(),
                                 seasoning.period = numeric()){
    
  Period <- NULL
  value <- NULL
  variable <- NULL
  
  cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
                    "#CC79A7", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                    "#0072B2", "#D55E00", "#CC79A7", "#000000")
  
  CashFlow = PassThroughAnalytics(bond.id = bond.id, 
                                  original.bal = 100000, 
                                  trade.date = trade.date, 
                                  settlement.date = settlement.date, 
                                  price = 100, 
                                  PrepaymentAssumption = "PPC",
                                  scenario.set = "NC",
                                  begin.cpr = begin.cpr, 
                                  end.cpr = end.cpr, 
                                  seasoning.period = seasoning.period)
  
  plotdata = as.data.frame(cbind(CashFlow@Period, 
                                 CashFlow@ScheduledPrin, 
                                 CashFlow@PrepaidPrin, 
                                 CashFlow@PassThroughInterest, 
                                 CashFlow@ServicingIncome,
                                 CashFlow@PMIPremium, 
                                 CashFlow@GFeePremium))
  
  colnames(plotdata) <- c("Period", "Scheduled Principal", 
                          "Prepaid Principal", "Pass-Through Interest", "Servicing", "PMI", "GFee")
  
  plotdata = melt(plotdata, id = "Period")
  
  Exhibit <- ggplot(plotdata, aes(x= Period, y = value, fill = variable)) +
    geom_area() +
    theme_minimal()+
    scale_fill_manual(values = cbbPalette) +
    labs(colour = "Legend", linetype = "Legend", x = "PoolAge (Mos.)", y = 
           "Pool Cash Flow per $100,000 Orig. Bal.") +
    theme(axis.title.y=element_text(angle = 90, size = 20)) +
    theme(axis.text.y = element_text(angle = 90, size = 15)) +
    theme(axis.title.x=element_text(angle = 0, size = 20)) +
    theme(axis.text.x = element_text(angle = 0, size = 15)) +
    theme(legend.position = c(.82,.73))+
    theme(axis.title.y = element_text(vjust = .3)) +
    theme(legend.background = element_rect(fill = "white")) +
    theme(legend.title=element_blank()) +
    theme(legend.text=element_text(size= 15))
  
  plot(Exhibit)
}