#' Structured Securities Valuation Framework Graphic
#' 
#'  The graphic for the valuation of structured securities
#' @import ggplot2
#' @import reshape2
#' @import grid
#' @import termstrc
#' @export
ValuationFramework <- function(){
  value <- NULL
  variable <- NULL

# =============================================
# Load Color Blind Pallette
# =============================================

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
                "#000000"
)

# ======================================================
# Pass values to the R software package termstruc
# =====================================================
SpotCurve <- spr_ns(c(4.26591, -3.54742, 2.46095, 7.49599), m = 1:30)
BaseCase <- forwardrates("ns", c(4.26591, -3.54742, 2.46095, 7.49599), m = 1:30)
Maturity <- as.numeric(seq(1:30))
Coupon <- SpotCurve - .25


# =====================================================
# Make Graph 4.1 using the curve data above
# =====================================================
graph_Data <- data.frame(Maturity = Maturity, 
                            CouponCurve = Coupon, 
                            SpotRateCurve = SpotCurve, 
                            ZVSpread = SpotCurve +.20, 
                            YTM = 3.25)

graph_Data <- melt(graph_Data, id = "Maturity")

framework <-
  ggplot(graph_Data, aes(x= Maturity, y = value, colour = variable))+
  geom_line(aes(linetype = variable), size = 1)+
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash")) +
  geom_vline(xintercept = 10)+
  theme_minimal()+
  ylab("Swap Rate") +
  xlab("Maturity (years)") +
  scale_colour_manual(values = cbbPalette) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 20)) +
  theme(panel.grid.major = element_line(size = .25, color = "grey")) +
  theme(legend.position=c(.80,.25), legend.background = element_rect(fill = "transparent", colour = "white"))+
  theme(legend.title=element_blank())+
  annotate("pointrange", x = 9, y = 2.95, ymin = 2.65, ymax = 3.25, colour = "black", size = .5)+
  annotate("text", x=4.35, y = 2.9, label = c("Nominal Spread")) +
  annotate("pointrange", x= 19, y = 3.78, ymin = 3.67, ymax = 3.89, colour = "black", size = .5) +
  geom_segment(aes(x= 1, y = 2.8, xend = 8.75, yend = 2.8), 
               arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("text", x=14.75, y = 3.85, label = c("ZV-Spread")) +
  geom_segment(aes(x= 12.5, y = 3.75, xend = 18.5, yend = 3.75), 
               arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("text", x=4.35, y = 4.25, label = c("Cheap Cash Flows\n PV Spot > PV YTM"), size = 4) +
  annotate("text", x=16, y = 4.25, label = c("Rich Cash Flows\n PV Spot < PV YTM"), size = 4) 

plot(framework)}