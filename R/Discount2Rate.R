#' Title
#'
#' @param DF
#' @param Compounding
#'
#' @return
#' @export
#'
#' @examples
Discount2Rate <- function(DF, T, Compounding = "CC"){

        if((toupper(Compounding)=="CC")|toupper(Compounding)=="Continuous"){
                ZeroRates <- -(1/T)*log(x = DF)
        }
        else if((toupper(Compounding)=="AC")|toupper(Compounding)=="Annual"){
                ZeroRates <- ((1/DF))^(1/T)-1
        }
        ZeroRates[is.na(ZeroRates)] <- 0
        return(ZeroRates)
}

