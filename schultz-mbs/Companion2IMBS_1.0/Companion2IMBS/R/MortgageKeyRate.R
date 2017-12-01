#' Plot the mortgage key rate duration
#' 
#' The mortgage key rate plot function defaults to PrepaymentAssumption = "MODEL"
#' and scenario.set = "NC"
#' @param bond.id a character vector either bondlabMBS4 or bondlabMBS55
#' @param original.bal a numeric value the pool original balance
#' @param price a numeric value the price of the bond
#' @param trade.date a character value the trade date
#' @param settlement.date a character value the settlement date
#' @importFrom BondLab PassThroughAnalytics
#' @import ggplot2
#' @examples PlotMtgKeyRates(bond.id = "bondlabMBS4", original.bal = 1000, 
#' price = 105.75, trade.date = "01-10-2013", settlement.date = "01-13-2013")
#' @export
PlotMtgKeyRates <- function(bond.id = character,
                           original.bal = numeric(),
                           price = numeric(),
                           trade.date = character,
                           settlement.date = character){
  Tenor <- NULL
  Duration <- NULL
  
  cbbPalette <- c("#E69F00", 
                  "#56B4E9", 
                  "#009E73", 
                  "#F0E442", 
                  "#0072B2", 
                  "#D55E00", 
                  "#CC79A7", 
                  "#000000",
                  "#E69F00", 
                  "#56B4E9", 
                  "#009E73", 
                  "#F0E442", 
                  "#0072B2", 
                  "#D55E00", 
                  "#CC79A7", 
                  "#000000")
  
                PassThroughAnalytics <-PassThroughAnalytics(bond.id = bond.id, 
                                                            original.bal = original.bal, 
                                                            price = price, 
                                                            trade.date = trade.date, 
                                                            settlement.date = settlement.date, 
                                                            PrepaymentAssumption = "MODEL",
                                                            scenario.set = c("NC"))

                Key.Rate.Duration <- cbind(PassThroughAnalytics@KeyRateTenor, PassThroughAnalytics@KeyRateDuration)
                colnames(Key.Rate.Duration) <- c("Tenor", "Duration")
                Key.Rate.Duration <- as.data.frame(Key.Rate.Duration)


  KeyRateDuration <-
  ggplot(Key.Rate.Duration, aes(x = as.factor(Tenor), y = Duration, fill = as.factor(Tenor))) +
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme(panel.grid.major = element_line(size = .25, color = "grey"))+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cbbPalette, guide = "none") +
  ylab("Key Rate Duration") +
  xlab("Key Rate Tenor (years)") +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 20))

plot(KeyRateDuration)}