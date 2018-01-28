ParseDate <- function(DateToParse, DateType = "European"){
        
        # Lubridate is dependency package in our package
        require(lubridate)
        
        # Supress warnings
        options(warn=-1)
        
        # Convert given the DateType input
        if(toupper(DateType)=="EUROPEAN"){
                date <- as.Date(parse_date_time(x = DateToParse,orders = c("dmy","ymd")))
        }else if(toupper(DateType)=="AMERICAN"){
                date <- as.Date(parse_date_time(x = DateToParse,orders = c("mdy","ymd")))
        }else{
                stop("Wrong DateType")
        }
        
        # Turn back on warnings
        options(warn=0)
        
        # Output warning in case we return NA value
        if(any(is.na(x = date))){warning("ParseDate did not return a Date")}

        return(date)
}