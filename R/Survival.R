#' Survival probability from Hazard rates
#'
#' @param SurvivalDate Date for surival chance
#' @param ValuationDate Date of calculation
#' @param Maturities Hazard rate maturities
#' @param Hazards Hazard rates
#' @param Convention Day count convention
#'
#' @description Function takes a given date and Hazard rate Term Structure and calculates
#    the Survival Probability up to the SurvivalDate given the term structure of hazard rates
#    We use the assumptions of piecewise constant hazard rates

#' @return A survival percentage up to a given Date
#'
#' @export
#'
#' @examples
survival <- function(SurvivalDate, ValuationDate, Maturities, Hazards, Convention = "act/360"){
        #

        # Extract EndDates before the SurvivalDate
        EndDates <- c(Maturities[Maturities < SurvivalDate],SurvivalDate)

        # Calculate per period the StartDate
        StartDates <- lag(x = EndDates)
        StartDates[1] <- ValuationDate

        # Put in frame
        Frame <- data_frame(EndDates = EndDates, StartDates = StartDates)

        # Calculate Yearfrac given the convention
        Frame$Frac <- ralibr::yearfrac(DateBegin = Frame$StartDates,DateEnd = Frame$EndDates,DayCountConv = Convention)

        # Assign the hazards for these discrete periods
        Frame$Hazards <- Hazards[1:length(Frame$Frac)]

        if(any(is.na(x = Frame$Hazards))){
                Frame$Hazards[which(is.na(x = Frame$Hazards))] <- Hazards[length(x = Hazards)]
        }

        # Cumulative Survival is "Q = exp(SumProd(T*Lambda))"
        survival <- exp(-sum(Frame$Frac*Frame$Hazards))

        return(survival)
}
attr( survival, "description" ) <- list(
        "Calculate Survival probability from Hazard rates",
        SurvivalDate="Survival probability date",
        ValuationDate="The Hazard rate term structure valuation date",
        Maturities="The Hazard rate term structure maturities",
        Hazards="The Hazard rate values",
        Convention="optional - Day count convention to use")
