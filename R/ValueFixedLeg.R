ValueFixedLeg <- function(Curve, EffectiveDate, MaturityDate, SecondLast = NULL, Notional = 10000000, ExchangeNotional = FALSE, Currency = "EUR", CouponRate = 0.01, Frequency = "A", DayCount = "ACT/360", DayConv = "MF", Direction = "PAY", Output = "DIRTY"){
        ## Valuation Function for a fixed leg of a standard swap
        ## Curve is a Nx2 matrix containing Dates and Discount Rates
        ## Effective Date and Maturity Date needs to be specified and SecondLast is optional
        ## Notional and Notional CCY
        ## CouponRate (In decimal form)
        ## Day Count Convention and Business Day Convention with defaults "ACT/360" and "Modified Following"
        ## Output specifies what we want the function to return (Dirty, Clean, DV01, Table, List)
        

        # Take ValDate as the first date in the Curve matrix (User needs to make sure this is correct)
        ValDate <- Curve[1,1]
        
        # Bump curve with the default 1 bp parallel increase
        CurveBumped <- BumpCurve(Curve = Curve)
        
        # Generate the payment dates from Effective to Maturity, optionally including ValDate and SecondLast
        Dates <- GenerateDates(EffectiveDate = EffectiveDate,
                                    MaturityDate = MaturityDate,
                                    SecondLast = SecondLast,
                                    CouponFreq = Frequency,
                                    BusDayConv = DayConv,
                                    ValDate = ValDate,
                                    Output = "Frame")
        
        DatesBegin <- Dates$StartDates
        DatesEnd <- Dates$EndDates
        
        N <- length(DatesBegin)

        DatesPay <- RollWeekday(Day = DatesEnd, BusDayConv = "f")
        
        DatesFrac <- Yearfrac(DateBegin = DatesBegin,DateEnd = DatesPay,DayCountConv = DayCount)
        
        # Calculate the discount factor for the Payment End Dates
        DiscountFactors <- Interpolate(Vector = Curve, Point = DatesEnd, method = "cs")
        DiscountFactorsBump <- Interpolate(Vector = CurveBumped, Point = DatesEnd, method = "cs")
        
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
        AccrualTime <- Yearfrac(DateBegin = DatesBegin[1], DateEnd = ValDate, DayCountConv = DayCount)
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