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
bump_curve <- function(Curve, Bump = 0.0001, IsDiscounts = TRUE){
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

#' Convert Discount Factors to Spot Rates
#'
#' @param DF A discount factor (numeric)
#' @param Compounding optional (default="CC") - The compounding frequency
#'
#' @return
#' @export
#'
#' @examples
discount_to_rate <- function(DF, T, Compounding = "CC"){

        if((toupper(Compounding)=="CC")|toupper(Compounding)=="Continuous"){
                ZeroRates <- -(1/T)*log(x = DF)
        }
        else if((toupper(Compounding)=="AC")|toupper(Compounding)=="Annual"){
                ZeroRates <- ((1/DF))^(1/T)-1
        }
        ZeroRates[is.na(ZeroRates)] <- 0
        return(ZeroRates)
}

#' Title
#'
#' @param Rate
#' @param Compounding
#'
#' @return
#' @export
#'
#' @examples
rate_to_discount <- function(Rate, T, Compounding = "CC"){

        if((toupper(Compounding)=="CC")|toupper(Compounding)=="Continuous"){
                Df <- exp(-Rate*T)
        }
        else if((toupper(Compounding)=="AC")|toupper(Compounding)=="Annual"){
                Df <- 1/(1+Rate)^T
        }
}
