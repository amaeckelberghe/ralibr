#' Title
#'
#' @param Curve
#' @param EffectiveDate
#' @param MaturityDate
#' @param SecondLast
#' @param Notional
#' @param ExchangeNotional
#' @param Currency
#' @param CouponRate
#' @param Frequency
#' @param DayCount
#' @param DayConv
#' @param Direction
#' @param Output
#'
#' @return
#' @export
#'
#' @importFrom dplyr lead
#' @importFrom dplyr data_frame
#'
#' @examples
value_fixed_leg <- function(
        Curve,
        ValDate,
        EffectiveDate,
        MaturityDate,
        SecondLast = NULL,
        Notional = 10000000,
        ExchangeNotional = FALSE,
        Currency = "EUR",
        CouponRate = 0.01,
        Frequency = "A",
        DayCount = "ACT/360",
        DayConv = "MF",
        Direction = "PAY",
        Output = "DIRTY"){

        ## Valuation Function for a fixed leg of a standard swap
        ## Curve is a Nx2 matrix containing Dates and Discount Rates
        ## Effective Date and Maturity Date needs to be specified and SecondLast is optional
        ## Notional and Notional CCY
        ## CouponRate (In decimal form)
        ## Day Count Convention and Business Day Convention with defaults "ACT/360" and "Modified Following"
        ## Output specifies what we want the function to return (Dirty, Clean, DV01, Table, List)


        # Take ValDate as the first date in the Curve matrix (User needs to make sure this is correct)
        # ValDate <- parse_date_internal(Curve[1,1])
        ValDate <- parse_date_internal(ValDate)

        # Bump curve with the default 1 bp parallel increase
        CurveBumped <- bump_curve(Curve = Curve)
        CurveBumped[,1] <- parse_date_internal(CurveBumped[,1])

        # Generate the payment dates from Effective to Maturity, optionally including ValDate and SecondLast
        Dates <- generate_dates(StartDate = EffectiveDate,
                                    EndDate = MaturityDate,
                                    SecondLast = SecondLast,
                                    CouponFreq = Frequency,
                                    BusDayConv = DayConv,
                                    ValDate = ValDate,
                                    Output = "Frame") # Output is in Date format !

        # Convert for internal use
        # Dates <- parse_date_internal(Dates)

        # Create two vectors (no conversion needed)
        DatesBegin <- Dates$StartDates
        DatesEnd <- Dates$EndDates

        N <- length(DatesBegin)

        # Roll and parse because roll_weekday is exported
        DatesPay <- parse_date_internal(roll_weekday(Day = DatesEnd, BusDayConv = DayConv))

        # Yearfraction
        DatesFrac <- yearfrac(DateBegin = DatesBegin,DateEnd = DatesPay,DayCountConv = DayCount)

        # Calculate the discount factor for the Payment End Dates
        DiscountFactors <- interpolate(X = Curve[,1],Y = Curve[,2], x = DatesEnd, method = "cs")
        DiscountFactorsBump <- interpolate(X = CurveBumped[,1], Y = CurveBumped[,2],x = DatesEnd, method = "cs")

        # Buy or Sell?
        if(toupper(Direction)=="PAY"){
                Sign <- rep(x = -1, N)
                Type <- rep(x = "Pay Fixed")
        }else{
                Sign <- rep(x = 1, N)
                Type <- rep(x = "Receive Fixed")
        }

        # Calculate actual coupon payments per period
        Rates               <- rep(x = CouponRate, N)
        Notionals           <- rep(x = Notional,N)
        Cashflow            <- Sign * Notionals * Rates * DatesFrac

        # Do we add an exchange of principals on the end?
        if(ExchangeNotional){
                Cashflow[N]   <- Cashflow[N] + Sign[N]*Notional
        }

        # Discount cashflows with the discount factor
        CashflowDisc         <- Cashflow * DiscountFactors
        CashflowDiscBump     <- Cashflow * DiscountFactorsBump

        # Sum of discounted cashflows (= Dirty Values)
        ValueDirty           <- sum(CashflowDisc)
        ValueDirtyBump       <- sum(CashflowDiscBump)

        # Create the cashflow table
        Table <- data_frame(Type = Type,
                            CCY = rep(Currency,N),
                            PeriodBegin = DatesBegin,
                            PeriodEnd = DatesEnd,
                            PeriodPay = DatesPay,
                            YearFraction = DatesFrac,
                            Notional = Notionals,
                            Rate = Rates,
                            Cashflow = Cashflow,
                            DiscountFactor = DiscountFactors,
                            CashflowDisc = CashflowDisc)

        # Accrual from last Reset to ValDate ?
        AccrualTime <- yearfrac(DateBegin = DatesBegin[1], DateEnd = ValDate, DayCountConv = DayCount)
        Accrual     <- Notional * CouponRate * AccrualTime * Sign[1]

        # Clean Values
        ValueClean <- ValueDirty - Accrual
        ValueCleanBump <- ValueDirtyBump - Accrual

        # DV01 (Based on Dirty)
        DV01 <- ValueDirtyBump-ValueDirty

        # What output type
        if(toupper(Output)=="CLEAN"){OutputValue <- ValueClean}
        else if(toupper(Output)=="DIRTY"){OutputValue <- ValueDirty}
        else if(toupper(Output)=="TABLE"){OutputValue <- Table}
        else if(toupper(Output)=="DV01"){OutputValue <- DV01}
        else if(toupper(Output)=="LIST1"){OutputValue <- list(ValueClean, DV01, Table)}
        else if(toupper(Output)=="LIST2"){OutputValue <- list(ValueDirty, DV01, Table)}

        # Output
        return(OutputValue)

}
# attr( value_fixed_leg, "description" ) <- list(
#         "Value a fixed leg",
#         Notional="Notional amount")


#' Title
#'
#' @param CurveDiscount
#' @param CurveTenor
#' @param EffectiveDate
#' @param MaturityDate
#' @param SecondLast
#' @param LastFix
#' @param Notional
#' @param ExchangeNotional
#' @param Currency
#' @param Tenor
#' @param Frequency
#' @param Spread
#' @param DayConv
#' @param DayCount
#' @param Direction
#' @param Output
#'
#' @return
#' @export
#'
#' @importFrom dplyr lead
#'
#' @examples
value_floating_leg <- function(
        CurveDiscount,
        CurveTenor,
        ValDate,
        EffectiveDate,
        MaturityDate,
        SecondLast = NULL,
        LastFix = 0,
        Notional = 10000000,
        ExchangeNotional = FALSE,
        Currency = "EUR",
        Tenor = "SA",
        Frequency = "SA",
        Spread = 0,
        DayConv = "MF",
        DayCount = "30/360",
        Direction = "RECEIVE",
        Output = "DIRTY"){

        ## Valuation Function for a FLOATING leg of a standard swap
        ## Curve is a Nx2 matrix containing Dates and Discount Rates
        ## Effective Date and Maturity Date needs to be specified and SecondLast is optional
        ## Notional and Notional CCY
        ## Tenor curve and Spread (with Last Fixing which is required!)
        ## Day Count Convention and Business Day Convention with defaults "30/360" and "Modified Following"
        ## Output specifies what we want the function to return (Dirty, Clean, DV01, Table, List)

        # Take ValDate from the Curve
        # ValDate <- parse_date_internal(CurveDiscount[1,1])
        ValDate <- parse_date_internal(ValDate)


        # Bump our curves with 1 bps parallel increase
        CurveDiscountBumped <- bump_curve(Curve = CurveDiscount)
        CurveDiscountBumped[,1] <- parse_date_internal(CurveDiscountBumped[,1])
        CurveTenorBumped <- bump_curve(Curve = CurveTenor)
        CurveTenorBumped <- parse_date_internal(CurveTenorBumped[,1])

        # Generate the Payment dates from Effective to Maturity, optionally including ValDate
        Dates <- generate_dates(StartDate = EffectiveDate,EndDate = MaturityDate,
                SecondLast = SecondLast,
                CouponFreq = Frequency,
                BusDayConv = DayConv,
                ValDate = ValDate,
                Output = "Frame") # Output is in R dates

        # Create two vectors
        DatesEnd <- Dates$EndDates
        DatesBegin <- Dates$StartDates

        N <- length(DatesBegin)

        # Roll and convert because roll_weekday is exported
        DatesPay <- parse_date_internal(roll_weekday(Day = DatesEnd, BusDayConv = DayConv))

        DatesFrac <- yearfrac(DateBegin = DatesBegin, DateEnd = DatesPay,DayCountConv = DayCount)

        # What are the discount factors for the payment period end dates?
        DiscountFactors <- interpolate(X = CurveDiscount[,1],Y = CurveDiscount[,2], x = DatesEnd, method = "cs")
        DiscountFactorsBump <- interpolate(X = CurveDiscountBumped[,1],Y = CurveDiscountBumped[,2],x = DatesEnd, method = "cs")


        # Do we pay or receive this leg?
        if(toupper(Direction)=="PAY"){
                Sign <- rep(x = -1,N)
                Type <- rep(x = "Pay Floating",N)
        }else{
                Sign <- rep(x = 1, N)
                Type <- rep(x = "Receive Floating", N)
        }

        # Calculate the forward rates given the current discount factors and add to last fixing
        if(N>1){
                FwdRates <- c(LastFix,
                        forward_rate(StartDate = DatesBegin[-1],
                                EndDate = DatesEnd[-1],
                                frame = CurveTenor,
                                DayCount = DayCount))
                FwdRatesBumped <- c(LastFix,
                        forward_rate(StartDate = DatesBegin[-1],
                                EndDate = DatesEnd[-1],
                                frame = CurveTenorBumped,
                                DayCount = DayCount))
        }else{
                FwdRates <- LastFix
                FwdRatesBumped <- LastFix
        }


        # Spread and Notionals ?
        Spreads <- rep(x = Spread,N)
        Notionals <- rep(x = Notional,N)

        # Calculate the actual coupon payments per payment period
        Cashflow <- Notionals * (FwdRates+Spreads) * Sign * DatesFrac
        CashflowBump <- Notionals * (FwdRatesBumped+Spreads) * Sign * DatesFrac

        # Principal Exchange
        if(ExchangeNotional){
                Cashflow[N]           <- Cashflow[N] + Sign[N]*Notional
                CashflowBump[N]       <- CashflowBump[N] + Sign[N]*Notional
        }

        # Value of the cashflows after discounting
        CashflowDisc            <- Cashflow * DiscountFactors
        CashflowBumpDisc        <- CashflowBump * DiscountFactorsBump

        # Sum of discounted cashflows (=Dirty Value)
        ValueDirty        <- sum(CashflowDisc)
        ValueDirtyBump  <- sum(CashflowBumpDisc)

        # The cashflow table with details
        Table <- data_frame(Type = Type,
                CCY = rep(Currency,N),
                PeriodBegin = DatesBegin,
                PeriodEnd = DatesEnd,
                PeriodPay = DatesPay,
                YearFraction = DatesFrac,
                Notional = Notionals,
                Rate = FwdRates + Spread,
                Cashflow = Cashflow,
                DiscountFactor = DiscountFactors,
                CashflowDisc = CashflowDisc)

        # Accrual
        AccrualTime <- yearfrac(DateBegin = DatesBegin[1], DateEnd = ValDate, DayCountConv = DayCount)
        Accrual <- Notional * FwdRates[1] * AccrualTime * Sign[1]

        # Clean values
        ValueClean <- ValueDirty - Accrual
        ValueCleanBump <- ValueDirtyBump - Accrual

        # DV01 (Based on Dirty)
        DV01 <- ValueDirtyBump-ValueDirty

        # What output type
        if(toupper(Output)=="CLEAN"){OutputValue <- ValueClean}
        if(toupper(Output)=="DIRTY"){OutputValue <- ValueDirty}
        if(toupper(Output)=="TABLE"){OutputValue <- Table}
        if(toupper(Output)=="DV01"){OutputValue <- DV01}
        if(toupper(Output)=="LIST1"){OutputValue <- list(ValueClean, DV01, Table)}
        if(toupper(Output)=="LIST2"){OutputValue <- list(ValueDirty, DV01, Table)}

        # Output
        return(OutputValue)
}
