#' Calculate fraction of years between two Dates
#'
#' @usage
#'
#' @param DateBegin The starting Date
#' @param DateEnd The ending Date
#' @param DayCountConv Day Count Convention for yearfraction calculation
#'    Default is ACT/360. See also 'Details'
#'
#' @return Numeric value representing years between Staring Date and Ending Date
#'
#' @details Dates can be inserted as character (DD-MM-YYYY or YYYY-MM-DD) and "-" is optional. (YYYYMMDD)
#'    Possible Day Count Convetions to use are "ACT/360", "ACT/365, "ACT/ACT", "30/360".
#'    The argument is case-insensitive and "/" is optional. ("ACT360",...)
#'
#' @export
#'
#' @examples yearfrac("2015-01-01","2020-01-01","act360")
yearfrac <- function(DateBegin,
                     DateEnd,
                     DayCountConv = "ACT/360"){


        DateBegin <- parse_date_internal(DateToParse = DateBegin)
        DateEnd <- parse_date_internal(DateToParse = DateEnd)

        # ACT360
        if(toupper(DayCountConv)=="ACT/360"|toupper(DayCountConv)=="ACT360"){
                Fracs <- (as.integer(DateEnd)-as.integer(DateBegin))/360
        }
        # ACT365
        else if(toupper(DayCountConv)=="ACT/365"|toupper(DayCountConv)=="ACT365"){
                Fracs <- (as.integer(DateEnd)-as.integer(DateBegin))/365
        }
        # ACTACT
        else if(toupper(DayCountConv)=="ACT/ACT"|toupper(DayCountConv)=="ACTACT"){
                # THIS HAS BEEN CORRECTED
                DateSpan = as.integer(DateEnd) - as.integer(DateBegin)
                year1 = as.integer(substring(DateBegin, 1, 4))
                year2 = as.integer(substring(DateEnd, 1, 4))
                YearSpan = year2 - year1 + 1
                LeapYear = rep(0, YearSpan)
                Years = year1:year2

                for (i in 1:length(LeapYear)) {
                        if (Years[i] - 4 * floor(Years[i] / 4) == 0)   {
                                if (Years[i] - 100 * floor(Years[i] / 100) == 0  &&
                                                Years[i] - 400 * floor(Years[i] / 400) == 1) {
                                        LeapYear[i] = 0
                                }
                                LeapYear[i] = 1
                        }
                }

                Percentage365 = sum(LeapYear == 0)/length(LeapYear)
                Percentage366 = sum(LeapYear == 1)/length(LeapYear)

                Fracs <- DateSpan/(365*Percentage365 + 366*Percentage366)
        }
        # 30/360
        else if(toupper(DayCountConv)=="30/360"|toupper(DayCountConv)=="30/360U"|toupper(DayCountConv)=="30360"){
                sd <- as.POSIXlt(x = DateBegin)
                ed <- as.POSIXlt(x = DateEnd)
                Fracs <- ((ed$year-sd$year)*360+(ed$mon-sd$mon)*30+(ed$mday-sd$mday))/360
        }
        else{
                stop("Wrong Day Count Convention")
        }

        return(Fracs)
}
attr( yearfrac, "description" ) <- list(
        "Calculate a Yearfraction between dates",
        DateBegin="Starting date",
        DateEnd="Ending date",
        DayCountConv="optional - Day count convention"
);



#' Generating sequence of Dates
#'
#' @param ValDate optional (default=NULL) - Cuts of the date sequence.
#' @param SecondLast optional (default=NULL) - Adjust the second to last observation (See details).
#' @param CouponFreq optional (default="M") - Frequency of observations (See details).
#' @param BusDayConv optional (default="F") - Business Day Convention (See details).
#' @param IMM optional (default=FALSE) - Generates IMM dates (See details).
#' @param Output optional (default="Vector"). Type of output requested (See details).
#' @param StartDate Starting Date of the sequence
#' @param EndDate Ending Date of the sequence
#'
#' @usage
#'
#' @details Generating sequency of dates moving backward from "End Date"
#'
#' @return A vector or dateframe of Dates
#'
#' @export
generate_dates <- function(StartDate,
                           EndDate,
                           ValDate = NULL,
                           SecondLast = NULL,
                           CouponFreq = "M",
                           BusDayConv = "F",
                           IMM = FALSE,
                           Output = "Vector"){

        # Function generates payment dates moving backward from the Maturity Date
        # Optional ValDate argument cuts off sequence just before ValDate (= Clean)

        # Parsing Date to correct R date format since function is exported.
        StartDate <- parse_date_internal(DateToParse = StartDate)
        EndDate <- parse_date_internal(DateToParse = EndDate)

        if(IMM){
                warning("unsure of correct functionality on IMM dates")
                CouponFreq <- "Q"
                EndDate <- next_imm(Date = EndDate)
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

        # slightly more complicated when secondlast is defined
        if(!is.null(SecondLast)){
                SecondLast <- parse_date_internal(DateToParse = SecondLast)
                Dates <- seq.Date(from = SecondLast,to = StartDate,by = Step)
                Dates <- c(EndDate,Dates,StartDate)
        }else{
                Dates <- seq.Date(from = EndDate,to = StartDate,by = Step)
        }

        # Manual override in case the EffectiveDate was not reached
        if(!any(Dates<=StartDate)){
                Dates <- c(Dates,StartDate)
        }

        # Roll Weekdays according to convention (Parse afterwards)
        Dates <- roll_weekday(Day = Dates, BusDayConv = BusDayConv)
        Dates <- parse_date_internal(Dates)

        # Remove Historical Dates before ValDate (but include last date (~clean price))
        if(!is.null(ValDate)){
                # Parse the ValDate also incase its in Excel numeric value
                ValDate <- parse_date_internal(DateToParse = ValDate)

                # Cut off historical cashflow (including T-1)
                if(ValDate > StartDate){
                        Dates <- Dates[1:which.max(Dates<ValDate)]
                }
        }

        # Sort unique dates
        Dates <- sort(unique(Dates))

        # Return output
        if(Output == "Vector"){

                # Export in excel numeric values
                Dates <- sapply(X = Dates,FUN = date_to_excel)

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
attr( generate_dates, "description" ) <- list(
        "Generate a sequence of dates",
        StartDate="Starting date for sequency",
        EndDate="Ending date for sequence",
        ValDate="optional - Valuation date",
        SecondLast="optional - Second to last date",
        CouponFreq="optional - Frequency of dates",
        BusDayConv="optional - Business day convention to apply to sequence of dates",
        IMM="Generate IMM business days",
        Ouput="Only use Vector in Excel"
);

#' Calculate the next IMM Date
#'
#' @param Date
#'
#' @return
#'
next_imm <- function(Date){
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


#' Parsing of dates to Excel numeric value
#'
#' @param DateToParse Input date value (integer/character/date)
#' @param DateType optional (default = "European") - format for DateToParse
#'
#' @return a Date in R date format
#'
#' @importFrom  lubridate parse_date_time
#'
parse_date_internal <- function(DateToParse, DateType = "European"){

        # Numerics are reserved for Excel integration and converted to R Dates.
        if(is.numeric(DateToParse)){DateToParse <- as.Date(x = DateToParse,origin = "1899-12-30")}

        # If DateToParse is a character we parse it with DateType formating
        if(is.character(x = DateType)){
                if(toupper(DateType)=="EUROPEAN"){
                        date <- as.Date(lubridate::parse_date_time(x = DateToParse,orders = c("dmy","ymd")))
                }else if(toupper(DateType)=="AMERICAN"){
                        date <- as.Date(lubridate::parse_date_time(x = DateToParse,orders = c("mdy","ymd")))
                }else{
                        stop("Wrong DateType")
                }
        }

        # If DateToParse is a Date then do nothing


        # Output warning in case we return NA value
        if(any(is.na(x = date))){warning("ParseDate failed to return a Date")}

        # We don't convert to Excel numeric values for internal use
        return(date)
}

#' Parsing of dates to Excel numeric value
#'
#' @param DateToParse Input date value (integer/character/date)
#' @param DateType optional (default = "European") - format for DateToParse
#'
#' @return a Date in Excel numeric value
#'
#' @export
#'
parse_date <- function(DateToParse, DateType = "European"){

        # Numerics are reserved for Excel integration
        if(is.numeric(DateToParse)){DateToParse <- as.Date(x = DateToParse,origin = "1899-12-30")}

        # If DateToParse is a character we parse it with DateType formating
        if(is.character(x = DateType)){
                if(toupper(DateType)=="EUROPEAN"){
                        date <- as.Date(lubridate::parse_date_time(x = DateToParse,orders = c("dmy","ymd")))
                }else if(toupper(DateType)=="AMERICAN"){
                        date <- as.Date(lubridate::parse_date_time(x = DateToParse,orders = c("mdy","ymd")))
                }else{
                        stop("Wrong DateType")
                }
        }

        # If DateToParse is a Date then do nothing


        # Output warning in case we return NA value
        if(any(is.na(x = date))){warning("ParseDate failed to return a Date")}

        # Exportable function so output is an Excel numeric value
        date <- date_to_excel(d1 = date)

        return(date)
}
attr( parse_date, "description" ) <- list(
        "Parsing of dates to Excel numeric value",
        DateToParse="Input date value (integer/character/date)",
        DateType="optional (default = European) - format for DateToParse"
);


#' Rolls a date for a given Business Day Convention
#'
#' @param Day A single Date or a vector of Dates
#' @param BusDayConv A specified business day convention
#'
#' @return A date in excel numeric format
#'
#' @export
#'
roll_weekday <- function(Day, BusDayConv="F"){


        # For BERT Integration reserve Numerics for Excel dates
        Day <- parse_date_internal(Day)

        # Function takes a Date or a list of dates and converts using the given Business Day Convention
        NewDay <- Day

        IdxSun = weekdays(x = Day, abbreviate=TRUE)=="zo"
        IdxSat = weekdays(x = Day, abbreviate=TRUE)=="za"

        # Convention Following
        if (toupper(BusDayConv)=="F"|toupper(BusDayConv)=="FOLLOWING"){

                NewDay[IdxSun] = Day[IdxSun] + 1
                NewDay[IdxSat] = Day[IdxSat] + 2
        }

        # Convention Modified Following
        else if (toupper(BusDayConv)=="MF"|toupper(BusDayConv)=="MODIFIEDFOLLOWING"){

                NewDay[IdxSun] = Day[IdxSun] + 1
                NewDay[IdxSat] = Day[IdxSat] + 2

                Idx = (months(NewDay)!=months(Day))
                NewDay[Idx] = NewDay[Idx] - 3
        }

        # Convention Preceding
        else if (toupper(BusDayConv)=="P"|toupper(BusDayConv)=="PRECEDING"){
                NewDay[IdxSun] = Day[IdxSun] - 2
                NewDay[IdxSat] = Day[IdxSat] - 1
        }

        # Change it to output an excel numeric value
        # Always parse_date afterwards when using in R
        NewDay <- date_to_excel(d1 = NewDay)

        return(NewDay)
}
attr( roll_weekday, "description" ) <- list(
        "Rolls a date for a given Business Day Convention",
        Day="a Date",
        BusDayConv="a Business Day Convention"
);




#' Convert a date in R to an excel numeric value
#'
#' @param d1 a Date in R
#'
#' @description Every function exported to the user that returns a date
#'    must return a date value in Excel numeric value.
#'
#' @return a numeric value representing an Excel date

date_to_excel <- function(d1){
        d <- as.numeric(d1 - as.Date(0, origin="1899-12-30", tz='UTC'))
        return(d)}


#' Rolling months
#'
#' @param Date a vector of Dates
#' @param Offset amount of months to roll
#'
#' @return a Date
#'
roll_month_vec <- function(Date,Offset){
        as.Date(sapply(Date, rollmomth, Offset), origin="1970-01-01")
}

#' Rolling months
#'
#' @param Date a Date
#' @param Offset amount of months to roll
#'
#' @return a Date
#'
roll_month <- function(Date,Offset){
        seq(Date, by = paste (Offset, "months"), length = 2)[2]
}
