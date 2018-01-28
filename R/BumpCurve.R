#' Title
#'
#' @param Curve
#' @param Bump
#' @param IsDiscounts
#'
#' @return
#' @export
#'
#' @examples
BumpCurve <- function(Curve, Bump = 0.0001, IsDiscounts = TRUE){
        # Take a single Curve and bump the Zero rates with 1 bps.
        # The default setting assumes that column 2 contains Discount factors
        # Output the new Discount curve (with dates)

        N <- length(Curve[,1])
        StartDates <- rep(x = Curve[1,1], N)
        T <- Yearfrac(DateBegin = StartDates,DateEnd = Curve[,1],DayCountConv = "act/360")
        if(IsDiscounts){
                ZeroRates <- Discount2Rate(DF = Curve[,2], T = T, Compounding = "CC")
                ZeroRatesBump <- ZeroRates + Bump
                CurveBump <- Curve; CurveBump[,2] <- Rate2Discount(Rate = ZeroRatesBump,T = T,Compounding = "CC")
        }else{
                CurveBump <- Curve
                CurveBump <- Curve[,2]+Bump
        }

        return(CurveBump)
}
