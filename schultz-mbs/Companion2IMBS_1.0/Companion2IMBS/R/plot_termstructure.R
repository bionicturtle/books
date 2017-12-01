
#' Plot Term Structure
#' 
#' A function to plot the term structure of interest rates
#' @param trade.date A character value the trade.date
#' @param method A character value the method passed to termstrc to fit the term structure suggest "ns"
#' @importFrom BondLab Rates
#' @importFrom BondLab TermStructure
#' @import ggplot2
#' @import termstrc
#' @import reshape2
#' @examples PlotTermStructure(trade.date = "01-10-2013", method = "ns")
#' @export
  PlotTermStructure <-  function(trade.date = character, method = character){
    
                        value <- NULL
                        variable <- NULL
    
                        #Notice for TermStructure to work the rates data object is called
                        #and passed to the TermStructure Function
                        rates.data = Rates(trade.date = trade.date)
                        Data = TermStructure(rates.data = rates.data, 
                                             method = method)
      
                        datatoplot = data.frame(cbind(Data@period[1:360], 
                                     Data@spotrate[1:360], 
                                     Data@forwardrate[1:360]))
      
                        colnames(datatoplot) =c("date", "spotrate", "forwardrate")
      
                        datatoplot = data.frame(melt(datatoplot, id = "date"))
      
              TSplot <- ggplot(datatoplot, aes(x = date, y = value, colour = variable)) +
              geom_line() +
              geom_line(size = 1.5) +
              guides(fill = guide_legend(title = "")) +
              ylab("Fixed Rate Payer Side") +
              xlab("Maturity (years)") + 
              theme_minimal() +
              scale_colour_grey(name = "", labels = c("Forward Curve", "Spot Curve")) +
              theme(legend.text = element_text(size = 20)) +
              theme(panel.grid.major = element_line(size = .25, color = "grey")) +
              theme(legend.position= "bottom", legend.background = element_rect(fill = "white", colour = "white"))+
              theme(axis.text = element_text(size = 15)) +
              theme(axis.title = element_text(size = 20))
      
      plot(TSplot)
    }
    
  
