#' Mortgage Total Return Graph
#' 
#' Calculates and plots mortgage pass-through total return analysis
#' This function illustrates the call to BondLab PassThroughAnalytics
#' for the bondlabMBS4 and bondlabMBS55.
#' @export
TotalReturn <- function(){
  
  Scenario <- NULL
  value <- NULL
  variable <- NULL
  
  cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                  "#D55E00", "#CC79A7", "#000000")
  
  bondlabMBS4 <- PassThroughAnalytics(bond.id = "bondlabMBS4", 
                                      original.bal = 100000, 
                                      price = 105.75, 
                                      trade.date = "01-10-2013", 
                                      settlement.date = "01-13-2013", 
                                      scenario.set = c("DA25", "NC", "UA50", "UA100", "UA150", "UA200"),
                                      PrepaymentAssumption = "MODEL")
  
  bondlabMBS55 <- PassThroughAnalytics(bond.id = "bondlabMBS55", 
                                       original.bal = 100000, 
                                       price = 107.5, 
                                       trade.date = "01-10-2013", 
                                       settlement.date = "01-13-2013", 
                                       scenario.set = c("DA25", "NC", "UA50", "UA100", "UA150", "UA200"),
                                       PrepaymentAssumption = "MODEL")
  
  MBS4.Scenario <- ExtractScenario(bond.id = bondlabMBS4)
  MBS55.Scenario <- ExtractScenario(bond.id = bondlabMBS55)
  
  
  Return.Data <- data.frame(cbind(as.numeric(MBS4.Scenario[,2]), 
                                  as.numeric(MBS4.Scenario[,8]), 
                                  as.numeric(MBS55.Scenario[,8])))
  
  colnames(Return.Data) <- c("Scenario", "MBS 4.00", "MBS 5.50")
  
  Return.Data <- melt(Return.Data, id = c("Scenario"))
  Return.Data$variable <- factor(Return.Data$variable, ordered = TRUE, levels = c("MBS 4.00", "MBS 5.50"))
  
  
  TotalReturnGraph <- 
    ggplot(Return.Data, aes(x = Scenario, y = value, colour = variable, shape = variable, linetype =  variable)) +
    geom_line(size = 1) +
    geom_point(size = 5, alpha = .75) +
    theme_minimal() +
    labs(colour = "Legend", shape = "Legend", linetype = "Legend", x = "Scenario Interest Rate Shift (bps)" , y = "Horizon Total Return") + 
    theme(panel.grid.major = element_line(size = .25, color = "grey")) +
    theme(axis.text = element_text(size = 15)) +
    theme(axis.title = element_text(size = 20)) +
    theme(plot.margin = unit(c(0, .75, 0, 1.25), "cm")) +
    theme(legend.position = c(.85,.90), legend.background = element_rect(colour = 'white', fill = 'white')) +
    theme(legend.text=element_text(size=15)) +
    scale_x_continuous(breaks = c(-25, 0, 50, 100, 150, 200 )) +
    scale_colour_manual(values = cbbPalette) 
  
  
    TotalReturnTable <- ggplot(Return.Data, aes(x = as.numeric(Scenario), y = factor(variable), label = round(value, digits = 2))) +
    geom_text(size = 5.5) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), panel.grid = element_blank(), 
          axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y =  element_text(size = rel(1.5))) +
    scale_y_discrete(labels = c("MBS 4.00", "MBS 5.50"))
  
    Layout <- grid.layout(nrow = 2, ncol = 1,
                        widths = unit(c(1), c("null", "null")),
                        heights = unit(c(2, .5), c("null", "null")) )
  
  
    vplayout <- function(...){
    grid.newpage()
    pushViewport(viewport(layout = Layout))
  }
  
  subplot <- function(x, y) {viewport(layout.pos.row = x,
                                     layout.pos.col = y)}
  mmplot <- function(a, b) {
    vplayout()
    print(a, vp = subplot(1, 1))
    print(b, vp = subplot(2, 1))
  }
  
  TotalReturn <- mmplot(TotalReturnGraph, TotalReturnTable)

}