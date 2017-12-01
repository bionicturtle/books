#' OAS Analaysis of REMIC
#' 
#' Function calls BondLab REMIC.OAS and plots the results
#' copyright Bond Lab Technologies, Inc 2015
#' @importFrom BondLab REMIC.OAS
#' @param bond.id a character string the bond id
#' @param trade.date a character string the trade date
#' @param settlement.date a character string the settlement date
#' @param tranche.price a numeric value the price
#' @param collateral.price a numeric value the price of the underlying collateral
#' @param sigma a numeric value the **annualized** volatility.  The volatilty assumption assumes
#' @param paths a numeric value the number of interest rate paths
#' a trading year of 252 days
#' @examples REMICOAS(bond.id = "BondLabSEQ1", trade.date = "01-10-2013", settlement.date = "01-13-2013",
#' tranche.price = 100, collateral.price = 100, sigma = 0.20, paths = 200)
#' @export
REMICOAS <- function(bond.id = character,
                            trade.date = character,
                            settlement.date = character,
                            tranche.price = numeric(),
                            collateral.price = numeric(),
                            sigma = numeric(),
                            paths = numeric()){
  value <- NULL
  ..density.. <- NULL
  
  set.seed(100)
  sigma = sigma/sqrt(trading.days)
  OAS.Analysis <- REMIC.OAS(bond.id = bond.id, 
                                 trade.date = trade.date, 
                                 settlement.date = settlement.date, 
                                 tranche.price = tranche.price,
                                 collateral.price = collateral.price,
                                 sigma = sigma, 
                                 paths = paths)
  
  # price distribution
  OAS.Price <- data.frame(OAS.Analysis@PriceDist)
  OAS.Price <- data.frame(cbind(OAS.Price, seq(1: length(OAS.Price))))
  colnames(OAS.Price) <- c("value", "count")
  
  price.dist <- ggplot(OAS.Price, aes(x = value )) +
    geom_density(fill = "#56B4E9", colour = "#56B4E9", alpha = .6) +
    geom_histogram(aes(y =..density..), color = "lightgrey", fill = "#0072B2", binwidth = .25) +
    theme_minimal() +
    #scale_x_continuous(breaks = seq(80,120, 5)) +
    labs(title = "Price Distribution") +
    ylab("Density")+
    xlab("Path Price") +
    theme(panel.grid.major = element_line(size = .25, color = "grey")) +
    theme(axis.text = element_text(size = 15)) +
    theme(axis.title = element_text(size = 20)) + 
    theme(legend.position = "none")
  
  # modified duration distribution
  OAS.Mdur <- data.frame(OAS.Analysis@PathModDur)
  OAS.Mdur <- data.frame(cbind(OAS.Mdur, seq(1:length(OAS.Mdur))))
  colnames(OAS.Mdur) <- c("value", "count")
  
  Mdur.dist <- ggplot(OAS.Mdur, aes(x = value )) +
    geom_density(fill = "#56B4E9", colour = "#56B4E9", alpha = .6) +
    geom_histogram(aes(y =..density..), color = "lightgrey", fill = "#0072B2", binwidth = 1/12) +
    theme_minimal() +
    #scale_x_continuous(breaks = seq(80,120, 5)) +
    labs(title = "Mod. Duration Distribution") +
    ylab("Density")+
    xlab("Path Mod. Duration") +
    theme(panel.grid.major = element_line(size = .25, color = "grey")) +
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
    grid.text("Bond Lab REMIC OAS", x = .1, y = .7, just = "left", vp = subplot(1,1))
    grid.text("Bond Id:", x = .1, y = .5, just = "left", vp = subplot(1,1))
    grid.text(bond.id, x = .7, y = .5, just = "left", vp = subplot(1,1))
    grid.text("Option Adjusted Spread:", x = .1, y = .3, just = "left", vp = subplot(1,1))
    grid.text(format(OAS.Analysis@OAS * 100, digits = 4), x = .7, y = .3, just = "left", vp = subplot(1,1))
    grid.text("Zero Volatility Spread:", x = .1, y = .1, just = "left", vp = subplot(1,1))
    grid.text(format(OAS.Analysis@ZVSpread * 100, digits = 4), x = .7, y = .1, just = "left", vp = subplot(1,1))
    print(a, vp = subplot(3, 1))
    print(b, vp = subplot(3, 2))
  }
  
  mmplot(price.dist, Mdur.dist)

} 