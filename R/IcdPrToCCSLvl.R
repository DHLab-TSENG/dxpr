#' Get the Multi-level Clinical Classifications Software (CCS) categories and description for ICD-9 and ICD-10 codes on procedures.
#'
#'  Multi-leve Clinical Classifications Software (CCS) for ICD-9 and ICD-10 procedure codes in clinical procedures data is a procedure categorization scheme. Four levels exist in the multi-level procedure CCS for ICD-9-CM codes, and two levels exist in the multi-level procedure CCS for ICD-10-CM codes
#'
#' return Multi-leve Clinical Classifications Software (CCS) categories or description based on ICD-9 and ICD-10 codes
#'
#' @import data.table
#' @param PrDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", "Date"
#' @param idColName A column for MemberID of PrDataFile
#' @param icdColName A column for ICD of PrDataFile
#' @param dateColName A column for Date of PrDataFile
#' @param icd10usingDate icd 10 using date
#' @param CCSLevel By default it is set to \code{1}.Clinical Classifications Software (CCS) multiple level
#' @param isDescription Clinical Classifications Software (CCS) multiple level categories/description for icd9/10. By default it is set to \code{True}.
#' @export
#' @source ICD-9-PCS CCS (2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source ICD-10-PCS CCS (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_pr_icd10pcs_2019_1.zip}
#' @examples
#' head(samplePrFile)
#' IcdPrToCCSLvl(samplePrFile, ID, ICD, Date, "2015-10-01", 2, TRUE)
#'
IcdPrToCCSLvl <- function(PrDataFile, idColName, icdColName, dateColName, icd10usingDate, CCSLevel = 1, isDescription = TRUE){
  PrDataFile <- as.data.table(PrDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  PrDataFile <- PrDataFile[,DataCol,with = FALSE]
  names(PrDataFile) <- c("ID","ICD", "Date")
  PrDataFile[,"Date"] <- as.Date(PrDataFile[,Date])
  PrDataFile[,Number:=1:nrow(PrDataFile)]
  PrDataFile[,Short:=IcdPrDecimalToShort(PrDataFile,ICD,Date,icd10usingDate)$ICD]

  if(isDescription == T){
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel, "_LABEL")
  }else{
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel)
  }

  if(CCSLevel <= 2){
    IcdToCCSLvl <- rbind(merge(PrDataFile[Date < icd10usingDate],ccsPrICD9[,c("ICD", CCSLvlCol), with = F],by.x ="Short",by.y = "ICD",all.x = T),
                         merge(PrDataFile[Date >= icd10usingDate],ccsPrICD10[,c("ICD", CCSLvlCol), with = F],by.x ="Short",by.y = "ICD",all.x = T))
  }else{
    IcdToCCSLvl <- merge(merge(PrDataFile[Date < icd10usingDate],ccsPrICD9[,c("ICD", CCSLvlCol), with = F],by.x ="Short",by.y = "ICD",all.x = T),
                         PrDataFile[Date >= icd10usingDate], by = names(PrDataFile), all = T)
  }
  IcdToCCSLvl <- IcdToCCSLvl[order(Number)][,eval(parse(text = paste(CCSLvlCol)))]
  IcdToCCSLvl
}
