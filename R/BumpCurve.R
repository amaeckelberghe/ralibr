#' Bump a discount curve
#'
#' @description Function takes a discount or zero-coupon curve and "bumps" it with a certain amount
#'
#' @param X,Y Vectors giving the coordinates of the points to be interpolated. X can optionally be "Date" format,
#'            allowing to interpolate on a typical discount curve structure
#'
#' @param x A single point specifying where to interpolate on observations of X. Can be numeric, Date
#'    or character ("YYYY-MM-DD")
#'
#' @param method Either linear ("l") or cubicspline ("cs")
#'
#' @return The value corresponding to x of values in Y.
#'
#' @examples
#' Interpolate(X = c(1,3,5),Y = c(100,200,300),x = 2,method="l")
#' Interpolate(Curve$Date, Curve$Discount_Factors, "2025-03-03", "linear")
#' @export

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
