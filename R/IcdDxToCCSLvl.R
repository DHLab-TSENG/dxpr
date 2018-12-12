if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "CCS_LVL_1",
  "CCS_LVL_1_LABEL",
  "CCS_LVL_2",
  "CCS_LVL_2_LABEL",
  "CCS_LVL_3",
  "CCS_LVL_3_LABEL",
  "CCS_LVL_4",
  "CCS_LVL_4_LABEL"))
#' Get the Multi-level Clinical Classifications Software (CCS) categories and description for ICD-9 and ICD-10 codes on diagnoses.
#'
#'  Multi-leve Clinical Classifications Software (CCS) for ICD-9 and ICD-10 diagnosis codes in clinical diagnostic data is a diagnosis categorization scheme. Four levels exist in the multi-level diagnosis CCS for ICD-9-CM codes, and two levels exist in the multi-level diagnosis CCS for ICD-10-CM codes
#'
#' return Multi-leve Clinical Classifications Software (CCS) categories or description based on ICD-9 and ICD-10 codes
#'
#' @import dplyr
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param CCSLevel Clinical Classifications Software (CCS) multiple level:1~4, CCS for ICD-10-CM only has 1~2 multiple levels
#' @param CCSLvlLabel Clinical Classifications Software (CCS) multiple level categories/description for icd9/10, default is True
#' @export
#' @source ICD-9-CM CCS (2012)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source ICD-10-CM CCS (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_dx_icd10cm_2019_1.zip}
#' @examples
#'
#' IcdDxToCCSLvl(sampleDxFile, ID, ICD, Date, "2015-10-01", 2, TRUE)
#'
IcdDxToCCSLvl <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, CCSLevel = 2, CCSLvlLabel = TRUE){
  DxDataFile <- DxDataFile[ , c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile$Date <- as.Date(DxDataFile$Date)
  DxDataFile$Number <- 1:nrow(DxDataFile)
  conversion <- IcdDxDecimalToShort(DxDataFile$ICD)
  DxDataFile$Short <- conversion$Short

  if(CCSLvlLabel == T){
    CCSLvlCol <- as.character(parse(text = paste("CCS_LVL_", CCSLevel, "_LABEL", sep = "")))
  }else{
    CCSLvlCol <- as.character(parse(text = paste("CCS_LVL_", CCSLevel, sep = "")))
  }
  if(CCSLevel <= 2){
    IcdToCCSLvl <- rbind(left_join(DxDataFile[DxDataFile$Date < icd10usingDate,],
                                   select_(ccsDxICD9, "ICD", CCSLvlCol), by = c("Short"="ICD")),
                         left_join(DxDataFile[DxDataFile$Date >= icd10usingDate,],
                                   select_(ccsDxICD10, "ICD", CCSLvlCol), by = c("Short"="ICD"))) %>% arrange(Number)
  }else{
    IcdToCCSLvl <- left_join(DxDataFile, left_join(DxDataFile[DxDataFile$Date < icd10usingDate,], select_(ccsDxICD9, "ICD", CCSLvlCol), by = c("Short"="ICD")),
                             by = names(DxDataFile)) %>% arrange(Number)
  }

  IcdToCCSLvlLong <- IcdToCCSLvl[!is.na(IcdToCCSLvl[,CCSLvlCol]),] %>%
    group_by_("ID",CCSLvlCol) %>%
    summarise(firstCaseDate = min(Date),
              endCaseDate = max(Date),
              period = endCaseDate - firstCaseDate,
              count = n())

  wrongFormat <- conversion$Error
  error_ICD <- anti_join(data.frame(ICD = IcdToCCSLvl$ICD[is.na(IcdToCCSLvl[,CCSLvlCol])],stringsAsFactors = F), wrongFormat, "ICD")
  if(anyNA(IcdToCCSLvl)){
    if(nrow(wrongFormat) > 0){
      message(paste0("wrong Format: ", unique(wrongFormat$ICD), sep = "\t\n"))
    }
    if(sum(is.na(IcdToCCSLvl)) > nrow(wrongFormat)){
      message(paste0("wrong ICD version: ", unique(error_ICD$ICD), sep = "\t\n"))
      message("\n")
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
    warning('"wrong ICD version" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = F)
  }
  return(list(groupedDf = IcdToCCSLvl,
              groupedData_Long = IcdToCCSLvlLong))
}
