Yearfrac <- function(DateBegin, DateEnd, DayCountConv){
        ## Yearfrac function calculates the yearfraction between dates based on a given convention
        
        # First check Dates formatting
        DateBegin <- ParseDate(DateBegin)
        DateEnd <- ParseDate(DateEnd)
        
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
