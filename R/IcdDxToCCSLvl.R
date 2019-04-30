#'
#' Get the Multi-level Clinical Classifications Software (CCS) categories and description for ICD-9 and ICD-10 codes on diagnoses.
#'
#'  Multi-leve Clinical Classifications Software (CCS) for ICD-9 and ICD-10 diagnosis codes in clinical diagnostic data is a diagnosis categorization scheme. Four levels exist in the multi-level diagnosis CCS for ICD-9-CM codes, and two levels exist in the multi-level diagnosis CCS for ICD-10-CM codes
#'
#' return Multi-leve Clinical Classifications Software (CCS) categories or description based on ICD-9 and ICD-10 codes
#'
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param CCSLevel By default it is set to \code{1}. Clinical Classifications Software (CCS) multiple level:1~4, CCS for ICD-10-CM only has 1~2 multiple levels
#' @param isDescription Clinical Classifications Software (CCS) multiple level categories/description for icd9/10. By default it is set to \code{True}.
#' @export
#' @source ICD-9-CM CCS (2012)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source ICD-10-CM CCS (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_dx_icd10cm_2019_1.zip}
#' @examples
#' head(sampleDxFile)
#' IcdDxToCCSLvl(sampleDxFile, ID, ICD, Date, "2015-10-01", 2, TRUE)
#'
IcdDxToCCSLvl <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, CCSLevel = 1, isDescription = TRUE){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])
  DxDataFile[,Number:=1:nrow(DxDataFile)]

  Conversion <- IcdDxDecimalToShort(DxDataFile,ICD,Date,icd10usingDate)
  DxDataFile[,Short:= Conversion$ICD]

  if(isDescription == T){
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel, "_LABEL")
  }else{
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel)
  }

  if(CCSLevel <= 2){
    IcdToCCSLvl <- rbind(merge(DxDataFile[Date < icd10usingDate],ccsDxICD9[,c("ICD", CCSLvlCol), with = F],by.x ="Short",by.y = "ICD",all.x = T),
                         merge(DxDataFile[Date >= icd10usingDate],ccsDxICD10[,c("ICD", CCSLvlCol), with = F],by.x ="Short",by.y = "ICD",all.x = T))
  }else{
    IcdToCCSLvl <- merge(merge(DxDataFile[Date < icd10usingDate],ccsDxICD9[,c("ICD", CCSLvlCol), with = F],by.x ="Short",by.y = "ICD",all.x = T),
                         DxDataFile[Date >= icd10usingDate], by = names(DxDataFile), all = T)
  }
  IcdToCCSLvl <- IcdToCCSLvl[order(Number),-"Number"]
  IcdToCCSLvlLong <- IcdToCCSLvl[!is.na(eval(parse(text = paste(CCSLvlCol)))),
                                 list(firstCaseDate = min(Date),
                                      endCaseDate = max(Date),
                                      count = .N),
                                 by = c("ID",CCSLvlCol)][,period := (endCaseDate - firstCaseDate),]

  return(list(groupedDT = IcdToCCSLvl,
              summarised_groupedDT = IcdToCCSLvlLong,
              Error = Conversion$Error))
}
