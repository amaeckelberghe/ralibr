Rate2Discount <- function(Rate, T, Compounding = "CC"){
        
        if((toupper(Compounding)=="CC")|toupper(Compounding)=="Continuous"){
                Df <- exp(-Rate*T)
        }
        else if((toupper(Compounding)=="AC")|toupper(Compounding)=="Annual"){
                Df <- 1/(1+Rate)^T
        }
}