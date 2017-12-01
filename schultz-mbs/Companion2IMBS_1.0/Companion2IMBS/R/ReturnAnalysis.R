#' Function illustrates basic return analyis
#' 
#'  cash flow analysis of fixed income return
#'  the function replicates Table 5.2
#'  @importFrom BondLab TimeValue
#'  @export
ReturnAnalysis <- function(){ 

# ==============================================
# Build the return matrix
# ==============================================
# Dimension an array 10 rows, 10 columns, and 1 table
# To store the cash flows and reinvestment amounts

arraysize <- c(10, 10, 1)
ReturnMatrix <- array(0, arraysize)

for(i in 1:nrow(ReturnMatrix)){
  ifelse(i != 10, ReturnMatrix[i,i,1] <- 15, ReturnMatrix[i,i,1] <- 1015) }

for( i in 1:nrow(ReturnMatrix)-1){
  for(j in (i + 1) :ncol(ReturnMatrix)){
    ReturnMatrix[i,j,1] <- TimeValue(interest.rate = 0.0025, 
                                     number.periods = 1, 
                                     frequency = 1, 
                                     type ="FV") * ReturnMatrix[i,j-1,1]
  }
}

# =============================================
# Calculate total coupon + reinvestment
# =============================================

Total_Coupon_plus_Reinvestment <- sum(ReturnMatrix[1:10, 10, 1]) - 1000

annualized_return <- AnnualizedReturn(BeginValue = 1000,
                                      EndValue = sum(ReturnMatrix[1:10, 10, 1]),
                                      TimeHorizon = 5,
                                      Frequency = 2)

return(ReturnMatrix)}