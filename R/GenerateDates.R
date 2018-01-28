GenerateDates <- function(EffectiveDate, MaturityDate, ValDate = NULL, SecondLast = NULL, CouponFreq = "Monthly", BusDayConv = "F", IMM = FALSE, Output = "Vector"){
        # Function generates payment dates moving backward from the Maturity Date
        # Optional ValDate argument cuts off sequence just before ValDate (= Clean)
        
        # Parsing Date to correct format
        EffectiveDate <- ParseDate(DateToParse = EffectiveDate)
        MaturityDate <- ParseDate(DateToParse = MaturityDate)
        
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
        
        if(IMM){
                CouponFreq <- "Q"
                MaturityDate <- NextIMM(Date = MaturityDate)
                SecondLast <- NULL # Ignore this if we use IMM dates
        }
        
        # Check if CouponFreq is numeric or string and convert accordingly
        if (is.character(CouponFreq)){
                if(toupper(CouponFreq)=="A"|toupper(CouponFreq)=="ANNUAL"){Step <- paste(-12, "month")}
                else if (toupper(CouponFreq)=="SA"|toupper(CouponFreq)=="SEMIANNUAL"){Step <- paste(-6, "month")}
                else if (toupper(CouponFreq)=="Q"|toupper(CouponFreq)=="QUARTERLY"){Step <- paste(-3, "month")}
                else if (toupper(CouponFreq)=="M"|toupper(CouponFreq)=="MONTHLY"){Step <- paste(-1, "month")}
                else if (toupper(CouponFreq)=="W"|toupper(CouponFreq)=="WEEKLY"){Step <- paste(-1, "week")}
                else if (toupper(CouponFreq)=="D"|toupper(CouponFreq)=="DAILY"){Step <- paste(-1, "day")}
                else (stop("Wrong CouponFreq input"))
        }
        else if (is.numeric(CouponFreq)){
                if(CouponFreq<=0){
                        stop("CouponFreq should be strictly positive")
                }
                Step = paste(round(-365/CouponFreq),"day") # has to be rounded to an integer
        }
        else{
                stop("Wrong Coupon Frequency input")
        }
        
        if(!is.null(SecondLast)){
                SecondLast <- ParseDate(DateToParse = SecondLast)
                Dates <- seq.Date(from = SecondLast,to = EffectiveDate,by = Step)
                Dates <- c(MaturityDate,Dates,EffectiveDate)
        }else{
                Dates <- seq.Date(from = MaturityDate,to = EffectiveDate,by = Step)
        }
        
        # Manual override in case the EffectiveDate was not reached
        if(!any(Dates<=EffectiveDate)){
                Dates <- c(Dates,EffectiveDate)
        }
        
        # Roll Weekdays according to convention
        Dates <- RollWeekday(Day = Dates,BusDayConv = BusDayConv)
        
        # Remove Historical Dates before ValDate (but include last date (~clean price))
        if(!is.null(ValDate)){
                # Parse the ValDate also
                ValDate <- ParseDate(DateToParse = ValDate)
                
                # Cut off historical cashflow (including T-1)
                if(ValDate > EffectiveDate){
                        Dates <- Dates[1:which.max(Dates<ValDate)]                        
                }
        }

        # Sort unique dates
        Dates <- sort(unique(Dates))
        
        # Return output
        if(Output == "Vector"){
                return(Dates)
        }
        if(Output == "Frame"){
                DatesEnd <- lead(x = Dates,n = 1)
                N <- length(Dates)
                frame <- data_frame(StartDates = Dates[-N],EndDates = DatesEnd[-N])
                return(frame)
        }
        else{
                stop("Wrong Output type. Choose 'Vector' or 'Frame'")
        }
}
