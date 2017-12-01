#' This function illustrates twist scenario analysis
#' 
#' TwistScenario plots the expected horizon return under each scenario.
#' TwistScenario is wrapper around PassThroughAnalytics see
#' the BondLab help file for more detail.  
#' @importFrom BondLab PassThroughAnalytics
#' @param bond.id a character string the bond id
#' @param original.bal a numeric value the original balance
#' @param price a numeric value the price
#' @param trade.date a character string the trade date
#' @param settlement.date a character string the settlement date
#' @param method a character the method used to fit the term strucure use method "ns"
#' @examples TwistScenario(bond.id = "bondlabMBS4", original.bal = 100000, 
#' price = 105.75, trade.date = "01-10-2013", settlement.date = "01-13-2013", method = "ns")
#' @export 
TwistScenario <- function(bond.id = "character",
                          original.bal = numeric(),
                          price = numeric(),
                          trade.date = "character",
                          settlement.date = "character",
                          method = "ns"){
  
        Scenario <- NULL
        Return <- NULL
        bondlabMBS4.Twist <- PassThroughAnalytics(bond.id = bond.id, 
                               original.bal = original.bal, 
                               price = price, 
                               trade.date = trade.date, 
                               settlement.date = settlement.date, 
                               PrepaymentAssumption = "MODEL", 
                               scenario.set = c("NC", "Steep50", "Flat50"),
                               method = "ns")
        
        ScenarioName <- rbind(bondlabMBS4.Twist@Scenario[[1]]@Name, 
                              bondlabMBS4.Twist@Scenario[[2]]@Name, 
                              bondlabMBS4.Twist@Scenario[[3]]@Name)
        HrzReturn <- rbind(bondlabMBS4.Twist@Scenario[[1]]@HorizonReturn,
                           bondlabMBS4.Twist@Scenario[[2]]@HorizonReturn,
                           bondlabMBS4.Twist@Scenario[[3]]@HorizonReturn)
        
        ReturnData <- data.frame(cbind(ScenarioName, round(HrzReturn, digits = 2)))
        colnames(ReturnData) <- c("Scenario", "Return")
        
        TwistPlot <-        ggplot(ReturnData, aes(x= Scenario, y = Return, fill = Return))+
                            geom_bar(stat = "identity") +
                            theme_minimal()+
                            theme(panel.grid.major = element_line(size = .25, color = "grey"))+
                            scale_fill_brewer(guide = "none") +
                            ylab("Return") +
                            xlab("Scenario") +
                            theme(axis.text = element_text(size = 15)) +
                            theme(axis.title = element_text(size = 20))
        
                plot(TwistPlot)

                            }