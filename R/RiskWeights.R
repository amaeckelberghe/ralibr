# These functions are designed to calculate the Basel III risk weights.
library(httr)
library(rvest)


# Function for rating for individual country
GetRating = function(Country, ECAI, Date){
  url = "https://countryeconomy.com/ratings/"
  countries = as.data.frame(unlist(html_attrs(html_nodes(content(x = RETRY(verb = "GET", url = url,times = 5)), css = "a"))))
  names(countries) = "Name"
  countries = countries$Name[which(substr(countries$Name,1,9) == "/ratings/")]
  countries = countries[-which(substr(countries,10,14) %in% c("fitch", "moody", "stand"))]
  AllCountries = c()
  for (i in 1: length(countries)){
    AllCountries[i] = sub(pattern = "/ratings/", replacement = "", x = as.character(countries[i]))
  }
  if(tolower(Country) %in% AllCountries){
    country = paste0(url, Country)
    ECAI_Number = ifelse(ECAI == "Moodys", 1, ifelse(ECAI == "S&P",2,3))
    Rating = as.data.frame(html_table(read_html(RETRY(verb = "GET", url = country, times = 3)), fill = TRUE)[[ECAI_Number]])[-c(1:2),]
    names(Rating) = c("LT - Foreign Ccy - Date", "LT - Foreign Ccy - Rating", "LT - Local Ccy - Date", "LT - Local Ccy - Rating", "ST - Foreign Ccy - Date",
                      "ST - Foreign Ccy - Rating", "ST - Local Ccy - Date", "ST - Local Ccy - Rating")
    Rating[,3] = as.Date(ifelse(Rating[,3] == "", NA, Rating[,3]))
    Output_Rating = ifelse(length(which(Rating[,3] < Date)) == 0, "NR", Rating[min(which(Rating[,3] < Date)),4])
  }else{
    Output_Rating = "NR"
  }
  return(Output_Rating)
}
# Examples
GetRating("Belgium", "Moodys", "2018-03-30")
GetRating(NA, "Moodys", "2018-03-30")




# Function to calculate RW of Exposures to Central Governments and Central Banks according to CRR Article 114
RiskWeightCentral = function(Country, ReportingDate, ECB = FALSE, ECAI = "S&P"){

  ReportingDate = as.Date(ReportingDate, origin = "1899-12-30")
  Rating = GetRating(Country, ECAI, ReportingDate)

  # Creating a dataframe to look up Credit Quality Step based on Rating
  RatingList = c("AAA","Aaa","AA+","Aa1","AA","Aa2","AA-","Aa3","A+","A1","A","A2","A-","A3","BBB+","Baa1","BBB","Baa2","BBB-","Baa3","BB+","Ba1","BB","Ba2","BB-","Ba3",
                 "B+","B1","B","B2","B-","B3","CCC+","Caa1","CCC","Caa2","CCC-","Caa3","CC","Ca","C")
  CreditQualityStepsList = c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6)
  CreditQualitySteps = as.data.frame(cbind(RatingList, CreditQualityStepsList))

  if(!(Rating %in% c(NA, "NR", ""))){
    # Rated Central Governments or Central Banks:

    # Specific Credit Quality Step for this exposure based on its rating
    CreditQualityStep = as.numeric(CreditQualitySteps[which(CreditQualitySteps$RatingList==Rating),2])
    RWList = c(0,0.2,0.5,1,1,1.5)
    RW = RWList[CreditQualityStep]
  }else{
    RW = 1
  }

  # List of Member States
  MemberStates = c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech-Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland",
                   "Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","UK")

  RW = ifelse(Country %in% MemberStates, 0, RW)
  RW = ifelse(ECB, 0, RW)
  return(RW)
}
# Examples
RiskWeightCentral("Belgium", "2018-04-30")
RiskWeightCentral("China", "2018-04-30")
RiskWeightCentral("Turkey", "2018-04-30")
RiskWeightCentral(NA, NA, ECB = TRUE)




# Function to calculate RW of Exposures to Institutions according to CRR Article 119, 120 & 121 (with no specific short-term assessment as laid out in Article 131)
RiskWeightInstitution = function(Rating, Country, ReportingDate, MaturityDate, ECAI = "S&P"){

  ReportingDate = as.Date(ReportingDate, origin = "1899-12-30")
  MaturityDate = as.Date(MaturityDate, origin = "1899-12-30")
  RemainingMaturity = as.numeric(MaturityDate - ReportingDate)

  # Creating a dataframe to look up Credit Quality Step based on Rating
  RatingList = c("AAA","Aaa","AA+","Aa1","AA","Aa2","AA-","Aa3","A+","A1","A","A2","A-","A3","BBB+","Baa1","BBB","Baa2","BBB-","Baa3","BB+","Ba1","BB","Ba2","BB-","Ba3",
                 "B+","B1","B","B2","B-","B3","CCC+","Caa1","CCC","Caa2","CCC-","Caa3","CC","Ca","C")
  CreditQualityStepsList = c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6)
  CreditQualitySteps = as.data.frame(cbind(RatingList, CreditQualityStepsList))

  if(!(Rating %in% c(NA, "NR", ""))){
    # Rated institutions:

    # Specific Credit Quality Step for this exposure based on its rating
    CreditQualityStep = as.numeric(CreditQualitySteps[which(CreditQualitySteps$RatingList==Rating),2])
      if(RemainingMaturity > 90){
        # Residual maturity of more than three months
        RWList = c(0.2,0.5,0.5,1,1,1.5)
        RW = RWList[CreditQualityStep]
      }else{
        # Residual maturity of less than three months
        RWList = c(0.2,0.2,0.2,0.5,0.5,1.5)
        RW = RWList[CreditQualityStep]
      }

  }else{
    # Unrated institutions:
    if(RemainingMaturity > 90){
      # Residual maturity of more than three months
      CountryRating = GetRating(Country, ECAI, ReportingDate)

      # Specific Credit Quality Step for this exposure based on its country rating
      CreditQualityStep = as.numeric(CreditQualitySteps[which(CreditQualitySteps$RatingList==CountryRating),2])
      RWList = c(0.2,0.5,1,1,1,1.5)
      RW = ifelse(is.na(CountryRating), 1, RWList[CreditQualityStep])

    }else{
      # Residual maturity of less than three months
      RW = 0.2
    }
  }

  return(RW)
}
# Examples
RiskWeightInstitution("NR", "Portugal", "2018-04-30", "2018-10-30")
RiskWeightInstitution("A", "Belgium", "2018-04-30", "2018-05-30")
RiskWeightInstitution("A", "Belgium", "2018-04-30", "2018-10-30")




# Function to calculate RW of Exposures to Corporates according to CRR Article 122
RiskWeightCorporate = function(Rating, Country, ReportingDate, ECAI = "S&P"){

  ReportingDate = as.Date(ReportingDate, origin = "1899-12-30")
  # Creating a dataframe to look up Credit Quality Step based on Rating
  RatingList = c("AAA","Aaa","AA+","Aa1","AA","Aa2","AA-","Aa3","A+","A1","A","A2","A-","A3","BBB+","Baa1","BBB","Baa2","BBB-","Baa3","BB+","Ba1","BB","Ba2","BB-","Ba3",
                 "B+","B1","B","B2","B-","B3","CCC+","Caa1","CCC","Caa2","CCC-","Caa3","CC","Ca","C")
  CreditQualityStepsList = c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6)
  CreditQualitySteps = as.data.frame(cbind(RatingList, CreditQualityStepsList))

  if(!(Rating %in% c(NA, "NR", ""))){
    # Rated Corporates:
    CreditQualityStep = as.numeric(CreditQualitySteps[which(CreditQualitySteps$RatingList==Rating),2])
    RWList = c(0.2,0.5,1,1,1.5,1.5)
    RW = RWList[CreditQualityStep]
  }else{
    # Unrated Corporates:
    CountryRating = GetRating(Country, ECAI, ReportingDate)
    CreditQualityStep = as.numeric(CreditQualitySteps[which(CreditQualitySteps$RatingList==CountryRating),2])
    RWList = c(0,0.2,0.5,1,1,1.5)
    RW = max(RWList[CreditQualityStep], 1)
  }

  return(RW)
}
# Examples
RiskWeightCorporate("AA", "Belgium", "2018-04-30")
RiskWeightCorporate("NR", "Belgium", "2018-04-30")




# Function to calculate RW of Retail Exposures according to CRR Article 123
RiskWeightRetail = function(){
  return(0.75)
}
# Example
RiskWeightRetail()




# Function to calculate RW of Exposures secured by mortgages on immovable property according to CRR Article 124, 125 & 126
RiskWeightImmovableProperty = function(Type, EAD, MV){

  if(Type == "Residential"){
    # Residential Property:
    Secured = max(0,min(0.8*MV, EAD))
    Unsecured = EAD - Secured
    RW = (Secured*0.35 + Unsecured*0.75)/EAD
  }else{
    # Commercial Property:
    Secured = max(0,min(0.5*MV, EAD))
    Unsecured = EAD - Secured
    RW = (Secured*0.5 + Unsecured*0.75)/EAD
  }
  return(RW)
}
# Examples
RiskWeightImmovableProperty("Commercial", 100000, 350000)
RiskWeightImmovableProperty("Commercial", 500000, 350000)
RiskWeightImmovableProperty("Residential", 500000, 350000)
RiskWeightImmovableProperty("Residential", 100000, 350000)
RiskWeightImmovableProperty("Residential", 100000, 0)




# Function to calculate RW of Exposures in Default according to CRR Article 127
RiskWeightDefault = function(ExposureValue, SpecificCreditRiskAdjustments, Secured = 0){
  Unsecured = ExposureValue - Secured
  ProvisionLevel = SpecificCreditRiskAdjustments / Unsecured
  RWSecured = 1
  RWUnsecured = ifelse(ProvisionLevel < 0.2, 1.5, 1)
  RW = (RWSecured*Secured + RWUnsecured*(Unsecured-SpecificCreditRiskAdjustments)) / ExposureValue
  return(RW)
}
# Examples
RiskWeightDefault(500000,500000)
RiskWeightDefault(500000,20000)
RiskWeightDefault(500000,20000, 300000)



Rating = "A+"
Country = "Belgium"
ReportingDate = "2018-04-30"
MaturityDate = "2018-07-30"
Type = "Residential"
EAD = 500000
MV = 350000
