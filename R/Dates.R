#' Calculate yearfraction between two Dates
#'
#' @usage
#' @param DateBegin The starting Date
#' @param DateEnd The ending Date
#' @param DayCountConv Day Count Convention for yearfraction calculation
#'    Default is ACT/360. See also 'Details'
#' @return Numeric value representing years between Staring Date and Ending Date
#' @details Dates can be inserted as character (DD-MM-YYYY or YYYY-MM-DD).
#'    Possible Day Count Convetions to use are "ACT/360", "ACT/365, "ACT/ACT", "30/360".
#'    The argument is case-insensitive and "/" is optional. ("ACT360",...)
#' @export
#' @examples yearfrac("2015-01-01","2020-01-01","act360")
yearfrac <- function(DateBegin,
                     DateEnd,
                     DayCountConv = "ACT/360"){

        # First check Dates formatting
        # R functionality only take Date class or character class
        # Excel functionality reserves the Numeric class
        if (is.numeric(DateBegin)){DateBegin <- as.Date(x = DateBegin,origin = "30-12-1899")}
        if (is.numeric(DateEnd)){DateEnd <- as.Date(x = DateEnd,origin = "30-12-1899")}
        if (is.character(DateBegin)){
                stop("Wrong Date format")
        }
        if (is.character(DateEnd)){
                stop("Wrong Date format")
        }
        #DateBegin <- parse_date(DateBegin)
        #DateEnd <- parse_date(DateEnd)

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
                ## THIS ONE IS SLIGHTLY WRONG
                DateSpan = as.integer(DateEnd)-as.integer(DateBegin)
                temp <- as.POSIXlt(DateBegin)
                temp$year <- temp$year+1
                DateAddYear <- as.Date(temp)
                YearSpan <- as.integer(DateAddYear)-as.integer(DateBegin)
                Fracs <- DateSpan / YearSpan
        }
        # 30/360
        else if(toupper(DayCountConv)=="30/360"|toupper(DayCountConv)=="30/360U"){
                sd <- as.POSIXlt(x = DateBegin)
                ed <- as.POSIXlt(x = DateEnd)
                Fracs <- ((ed$year-sd$year)*360+(ed$mon-sd$mon)*30+(ed$mday-sd$mday))/360
        }
        else{
                stop("Wrong Day Count Convention")
        }

        return(Fracs)
}



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
#'
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

        # Parsing Date to correct format
        StartDate <- parse_date(DateToParse = StartDate)
        EndDate <- parse_date(DateToParse = EndDate)

        if(IMM){
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

        if(!is.null(SecondLast)){
                SecondLast <- parse_date(DateToParse = SecondLast)
                Dates <- seq.Date(from = SecondLast,to = StartDate,by = Step)
                Dates <- c(EndDate,Dates,StartDate)
        }else{
                Dates <- seq.Date(from = EndDate,to = StartDate,by = Step)
        }

        # Manual override in case the EffectiveDate was not reached
        if(!any(Dates<=StartDate)){
                Dates <- c(Dates,StartDate)
        }

        # Roll Weekdays according to convention
        Dates <- RollWeekday(Day = Dates,BusDayConv = BusDayConv)

        # Remove Historical Dates before ValDate (but include last date (~clean price))
        if(!is.null(ValDate)){
                # Parse the ValDate also
                ValDate <- parse_date(DateToParse = ValDate)

                # Cut off historical cashflow (including T-1)
                if(ValDate > StartDate){
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


#' Parsing character to Dates class
#'
#' @param DateToParse a character representing a date
#' @param DateType optional (default="European") - Format of DateToParse
#'
#' @return a Date
#'
#' @import lubridate:::date_parse_time
#'
parse_date <- function(DateToParse,
        DateType = "European"){

        if(toupper(DateType)=="EUROPEAN"){
                date <- as.Date(lubridate::parse_date_time(x = DateToParse,orders = c("dmy","ymd")))
        }else if(toupper(DateType)=="AMERICAN"){
                date <- as.Date(lubridate::parse_date_time(x = DateToParse,orders = c("mdy","ymd")))
        }else{
                stop("Wrong DateType")
        }

        # Output warning in case we return NA value
        if(any(is.na(x = date))){warning("ParseDate failed to return a Date")}

        return(date)
}


#' Rolls a date for a given Business Day Convention
#'
#' @param Day A single Date or a vector of Dates
#' @param BusDayConv A specified business day convention
#'
#' @return A Date
#'
#' @export
#'
roll_weekday <- function(Day,
        BusDayConv="F"){
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
        return(NewDay)
}

roll_month_vec <- function(Date,Offset){as.Date(sapply(Date, rollmomth, Offset), origin="1970-01-01")}
roll_month <- function(Date,Offset){seq(Date, by = paste (Offset, "months"), length = 2)[2]}
