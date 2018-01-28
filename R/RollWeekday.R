RollWeekday <- function(Day, BusDayConv="F"){
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