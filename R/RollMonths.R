RollMonthsVec <- function(Date,Offset){as.Date(sapply(Date, RollMonths, Offset), origin="1970-01-01")}
RollMonths <- function(Date,Offset){seq(Date, by = paste (Offset, "months"), length = 2)[2]}
