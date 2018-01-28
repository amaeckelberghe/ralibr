Survival <- function(SurvivalDate, ValuationDate, Maturities, Hazards, Convention = "act/360"){
        # Function takes a given date and Hazard rate Term Structure and calculates
        # the Survival Probability up to the SurvivalDate given the term structure of hazard rates
        # We use the assumptions of piecewise constant hazard rates
        
        # Extract EndDates before the SurvivalDate
        EndDates <- c(Maturities[Maturities < SurvivalDate],SurvivalDate)
        
        # Calculate per period the StartDate
        StartDates <- lag(x = EndDates)
        StartDates[1] <- ValuationDate
        
        # Put in frame
        Frame <- data_frame(EndDates = EndDates, StartDates = StartDates)

        # Calculate Yearfrac given the convention
        Frame$Frac <- Yearfrac(DateBegin = Frame$StartDates,DateEnd = Frame$EndDates,DayCountConv = Convention)
        
        # Assign the hazards for these discrete periods
        Frame$Hazards <- Hazards[1:length(Frame$Frac)]
        
        if(any(is.na(x = Frame$Hazards))){
                Frame$Hazards[which(is.na(x = Frame$Hazards))] <- Hazards[length(x = Hazards)]
        }
        
        # Cumulative Survival is "Q = exp(SumProd(T*Lambda))"
        Survival <- exp(-sum(Frame$Frac*Frame$Hazards))
        
        return(Survival)
}