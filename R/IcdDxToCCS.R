if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "ccsDxICD9",
  "CCS_CATEGORY",
  "CCS_CATEGORY_DESCRIPTION",
  "icd10usingDate",
  "ccsDxICD10"))
#' Get the Clinical Classifications Software (CCS) categories and description for ICD-9 and ICD-10 codes on diagnoses.
#'
#' Clinical Classifications Software (CCS) for ICD-9 and ICD-10 diagnosis codes in clinical diagnostic data is a diagnosis categorization scheme.
#'
#' return Clinical Classifications Software (CCS) categories or description based on ICD-9 and ICD-10 codes
#'
#' @import dplyr
#' @importFrom stats complete.cases
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate ICD-10 using date
#' @param isCCSCategoryDescription  Clinical Classifications Software (CCS) single level categories/description for ICD-9 or ICD-10, default is True
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
#' IcdDxToCCS(DxDataFile, ID, ICD, Date, "2016-01-01", TRUE)
#'
IcdDxToCCS <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, isCCSCategoryDescription = TRUE){
  DxDataFile <- DxDataFile[, c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile <- DxDataFile %>% mutate(Number =  1:nrow(DxDataFile))
  DxDataFile_allShort <-DxDataFile
  DxDataFile_allShort$ICD <- IcdDxDecimaltoShort(DxDataFile_allShort$ICD)$Short

  icd9ToCCS <- inner_join(DxDataFile_allShort[as.Date(DxDataFile_allShort$Date) < icd10usingDate,],
                          select(ccsDxICD9, ICD, CCS_CATEGORY, CCS_CATEGORY_DESCRIPTION), by = "ICD")
  icd10ToCCS <- inner_join(DxDataFile_allShort[as.Date(DxDataFile_allShort$Date) >= icd10usingDate,],
                           select(ccsDxICD10, ICD, CCS_CATEGORY, CCS_CATEGORY_DESCRIPTION), by = "ICD")

  CCS_combine <- left_join(DxDataFile_allShort, rbind(icd9ToCCS, icd10ToCCS), by = names(DxDataFile_allShort))

  if (isCCSCategoryDescription == T) {
    IcdToCCS <- CCS_combine$CCS_CATEGORY_DESCRIPTION
  }else {
    IcdToCCS <- CCS_combine$CCS_CATEGORY
  }
  WrongFormat <- IcdDxDecimaltoShort(DxDataFile$ICD)$Error
  error_ICD <- anti_join(DxDataFile[is.na(IcdToCCS),], WrongFormat, "Number")

  if(anyNA(IcdToCCS)){
    if(length(WrongFormat) > 0){
      message(paste0("wrong Format: ", unique(WrongFormat$ICD), sep = "\t\n"))
    }
    if(sum(is.na(IcdToCCS)) > nrow(WrongFormat)){
      message(paste0("warning ICD: ", unique(error_ICD$ICD), sep = "\t\n"))
      message("\n")
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
    warning('"warning ICD" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = F)
  }
  IcdToCCS
}
