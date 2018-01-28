#' Title
#'
#' @param MarketData
#' @param ZeroData
#' @param ValuationDate
#' @param Convention
#' @param OutputDates
#' @param LGD
#' @param Output
#'
#' @return
#' @export
#'
#' @examples
BootstrapCDS <- function(MarketData, ZeroData, ValuationDate, Convention, OutputDates = NULL, LGD, Output = "Hazard"){
        # Function loops over every Maturity in the MarketData matrix and bootstraps a Hazard rate implied by it
        # MarketData needs to be an Nx2 Matrix containing Maturities (integers) and Spreads (in bps)
        # ZeroData needs to be a Curve as an Nx2 Matrix containing Dates and Discount factors

        # Initiliaze local functions to use in this function
        BootstrapQuotes <- function(Hazard, i, ValDate, Quotes, Curve, Convention, LGD){

                # First put the Hazard (Parameter to optimize) in the Quotes
                Quotes$Hazards[i] <- Hazard

                # Bootstrap the maturity given the previously calculated Hazards unchanged
                Frame <- GenerateDates(EffectiveDate = "1970-01-01",
                                       MaturityDate = Quotes$CDS.Maturities[i],
                                       ValDate = ValDate,
                                       IMM = TRUE,
                                       BusDayConv = "f",
                                       Output = "Frame")

                # ISDA convention is "following"
                Frame$EndDates <- RollWeekday(Day = Frame$EndDates)

                # Frac is act/360 as by ISDA convention
                Frame$Frac <- Yearfrac(DateBegin = Frame$StartDates,
                                       DateEnd = Frame$EndDates,
                                       DayCountConv = "act/360")

                # First period is "Riskless" before ValDate
                Frame$Frac[1] <- Yearfrac(DateBegin = ValDate,
                                          DateEnd = Frame$EndDates[1],
                                          DayCountConv = "act/360")

                # Frac2 using user-input Convention? (For Survival)
                Frame$Frac2 <- Yearfrac(DateBegin = Frame$StartDates,
                                        DateEnd = Frame$EndDates,
                                        DayCountConv = Convention)

                # First period is "Riskless" before ValDate
                Frame$Frac2[1] <- Yearfrac(DateBegin = ValDate,
                                           DateEnd = Frame$EndDates[1],
                                           DayCountConv = Convention)

                # CDS - running spread for contract duration
                Frame$Spread <- rep(x = Quotes$CDS.Spread[i]/10000,
                                    times = length(Frame$EndDates))

                Frame$EndDiscount <- Interpolate(Vector = Curve,
                                                 Point = Frame$EndDates,
                                                 method = "cs")

                Frame$Hazards <- sapply(X = Frame$EndDates,
                                        FUN = function(x,y) return(Quotes$Hazards[which.max(y>=x)]),
                                        y = Quotes$CDS.Maturities.IMM)

                # We are looping over here -> Try to find an efficient method
                Frame$Survival = NA
                for (i in 1:length(Frame$StartDates)){
                        Frame$Survival[i] <- exp(-sum(Frame$Frac2[1:i]*Frame$Hazards[1:i]))
                }

                Frame <- mutate(.data = Frame,CumulDefault = 1 - Survival)
                Frame <- mutate(.data = Frame,PeriodDefault = c(1-Survival[1],diff(Survival)*-1))

                # Premium Leg
                Frame <- mutate(.data = Frame,Premium = Frac*Spread*Survival*EndDiscount)

                # Add Accrued premia here
                Frame <- mutate(.data = Frame,Accrued = Frac*Spread*EndDiscount*PeriodDefault)

                # Protection Leg
                Frame <- mutate(.data = Frame,Protection = LGD*EndDiscount*PeriodDefault)

                # Return TestValue
                TestValue <- ((sum(Frame$Premium)+sum(Frame$Accrued))*10000 - sum(Frame$Protection)*10000)^2

                return(TestValue)
        }

        NextIMM <- function(Date){
                Month <- month(Date)
                Year <- year(Date)
                if((Month%%3==0)&(day(x = Date)>20)){
                        Month <- ceiling(x = Month/3)*3 + 3
                        if(Month>12){
                                Month <- Month - 12
                                Year <- Year + 1}
                }else{
                        Month <- ceiling(x = Month/3)*3
                }
                Day <- 20
                Month <- ceiling(x = Month/3)*3
                Date <- dmy(paste0(Day,"-",Month,"-",Year))
                return(Date)
        }

        ## Actual function
        # First Check Output string
        if(!any(c(Output=="Hazard",Output=="Survival",Output=="Default"))){stop("Wrong Output Type")}

        # Put ValuationDate in Date
        ValuationDate <- ParseDate(x = ValuationDate)

        # The idea is to solve the Hazards recursively
        MarketData$Hazards <- rep(x = 0.01,times = length(MarketData[,1]))

        # Tenors to Maturities (= Dates)
        MarketData$CDS.Maturities <- as.Date(mapply(FUN = RollMonths,12*MarketData[,1],Date = ValuationDate),origin = "1970-01-01")

        # ISDA Convention = "Following"
        MarketData$CDS.Maturities <- RollWeekday(Day = MarketData$CDS.Maturities,BusDayConv = "F")

        MarketData$CDS.Maturities.IMM <- as.Date(x = sapply(X = MarketData$CDS.Maturities,FUN = NextIMM),origin = "1970-01-01")
        MarketData$CDS.Maturities.IMM <- RollWeekday(Day = MarketData$CDS.Maturities.IMM,BusDayConv = "F")

        # If OutputDates = NULL then assign them the Maturities
        if(is.null(x = OutputDates)){OutputDates <- MarketData$CDS.Maturities.IMM}

        # Length of the Term Structure
        N <- length(MarketData[,1])

        # Initial Value for Hazard Rate in bootstrapping
        HazardStart <- 0.01

        # Loop over every point in the CDS Term Structure to bootstrap a hazard rate
        for(i in 1:N){
                result <- optim(par = HazardStart,
                                fn = BootstrapQuotes,
                                i = i,
                                ValDate = ValuationDate,
                                Quotes = MarketData,
                                Curve = ZeroData,
                                Convention = Convention,
                                LGD = LGD,
                                method = "Brent",
                                lower = 0,
                                upper = 1,control = list(abstol = 1e-18, reltol = 1e-10))

                MarketData$Hazards[i] <- result$par
        }


        # Select correct Output and return
        if(Output=="Hazard"){
                # Currently for hazards just return them with the Original Dates
                return(MarketData[,c(5,3)])
        }else{
                # For both options we need the Survival rates
                Survivals <- sapply(X = OutputDates,FUN = Survival,
                                    ValuationDate = ValDate,
                                    Maturities = MarketData$CDS.Maturities,
                                    Hazards = MarketData$Hazards,
                                    Convention = Convention)

                OutputFrame <- data_frame(Maturities = OutputDates)

                if(Output=="Survival"){
                        OutputFrame$Survivals <- Survivals
                        return(OutputFrame)
                }
                if(Output=="Default"){
                        Defaults <- 1 - Survivals
                        OutputFrame$Defaults <- Defaults
                        return(OutputFrame)
                }
        }
}
