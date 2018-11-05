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
#' @param CCSLevel Clinical Classifications Software (CCS) multiple level
#' @param CCSLvlLabel Clinical Classifications Software (CCS) multiple level categories/description for icd9/10, default is True
#' @export
#' @source ICD-9-CM CCS (2012)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source ICD-10-CM CCS (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_dx_icd10cm_2019_1.zip}
#' @examples
#' DxDataFile <- data.frame(ID=c("A","A","A"),
#'                          ICD=c("6929","V433","I350"),
#'                          Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10")),
#'                          stringsAsFactors = FALSE)
#' IcdDxToCCSLvl(DxDataFile, ID, ICD, Date, "2016-01-01", 2, TRUE)
#'
IcdDxToCCSLvl <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, CCSLevel = 1, CCSLvlLabel = TRUE){
  DxDataFile <- DxDataFile[ , c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile <- DxDataFile %>% mutate(Number =  1:nrow(DxDataFile))
  Conversion <- IcdDxDecimaltoShort(DxDataFile$ICD)
  DxDataFile$Short <- Conversion$Short

  icd9 <- DxDataFile[as.Date(DxDataFile$Date) < icd10usingDate,]
  icd10 <- DxDataFile[as.Date(DxDataFile$Date) >= icd10usingDate,]

  if(CCSLevel <= 2){
    icd9ToCCSLvl <- inner_join(icd9, select(ccsDxICD9, ICD, CCS_LVL_1, CCS_LVL_1_LABEL, CCS_LVL_2, CCS_LVL_2_LABEL), by = c("Short"="ICD"))
    icd10ToCCSLvl <- inner_join(icd10, select(ccsDxICD10, ICD, CCS_LVL_1, CCS_LVL_1_LABEL, CCS_LVL_2, CCS_LVL_2_LABEL), by = c("Short"="ICD"))
    CCSLvl_combine <- left_join(DxDataFile,
                                rbind(icd9ToCCSLvl, icd10ToCCSLvl),
                                by = names(DxDataFile))
  }else{
    icd9ToCCSLvl <- inner_join(icd9, ccsDxICD9, by = c("Short"="ICD"))
    icd10ToCCSLvl <- inner_join(icd10, ccsDxICD10, by = c("Short"="ICD"))
    CCSLvl_combine <- left_join(DxDataFile,
                                right_join(icd9ToCCSLvl, icd10ToCCSLvl, by = names(icd10ToCCSLvl)),
                                by = names(DxDataFile))
  }
  if(CCSLvlLabel == T){
    CCSLevelcol <- as.character(parse(text = paste("CCS_LVL_", CCSLevel, "_LABEL", sep = "")))
  }else{
    CCSLevelcol <- as.character(parse(text = paste("CCS_LVL_", CCSLevel, sep = "")))
  }
  IcdToCCSLevel <- CCSLvl_combine[, CCSLevelcol]

  WrongFormat <- Conversion$Error
  error_ICD <- anti_join(data.frame(ICD = CCSLvl_combine$ICD[is.na(IcdToCCSLevel)],stringsAsFactors = F), WrongFormat, "ICD")

  if(anyNA(IcdToCCSLevel)){
    if(nrow(WrongFormat) > 0){
      message(paste0("wrong Format: ", unique(WrongFormat$ICD), sep = "\t\n"))
    }
    if(sum(is.na(IcdToCCSLevel)) > nrow(WrongFormat)){
      message(paste0("wrong ICD version: ",unique(error_ICD$ICD), sep = "\t\n"))
      message("\n")
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
    warning('"wrong ICD version" means the ICD classify to wrong ICD version (cause the "icd10usingDate"), ICD-10  CCS multiple levels are 1~2 or other issues', call. = F)
  }
  IcdToCCSLevel
}
