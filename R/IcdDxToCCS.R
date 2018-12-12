if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "ccsDxICD9",
  "CCS_CATEGORY",
  "CCS_CATEGORY_DESCRIPTION",
  "icd10usingDate",
  "ccsDxICD10",
  "isCCSCategoryDescription"))
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
#' @source ICD-9-CM CCS (2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source ICD-10-CM CCS (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_dx_icd10cm_2019_1.zip}
#' @examples
#'
#' IcdDxToCCS(sampleDxFile, ID, ICD, Date, "2015-10-01", TRUE)$groupedDf
#' IcdDxToCCS(sampleDxFile, ID, ICD, Date, "2015-10-01", TRUE)$groupedData_Long
#'
IcdDxToCCS <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, isCCSCategoryDescription = T){
  DxDataFile <- DxDataFile[, c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile$Date <- as.Date(DxDataFile$Date)
  DxDataFile$Number <- 1:nrow(DxDataFile)
  conversion <- IcdDxDecimalToShort(DxDataFile$ICD)
  DxDataFile$Short <- conversion$Short

  if (isCCSCategoryDescription == T) {
    ccs_col <- "CCS_CATEGORY_DESCRIPTION"
  }else {
    ccs_col <- "CCS_CATEGORY"
  }
  IcdToCCS <- rbind(left_join(DxDataFile[DxDataFile$Date < icd10usingDate,],
                              select_(ccsDxICD9, "ICD", ccs_col), by = c("Short"="ICD")),
                    left_join(DxDataFile[DxDataFile$Date >= icd10usingDate,],
                              select_(ccsDxICD10, "ICD",ccs_col), by = c("Short"="ICD"))) %>% arrange(Number)

  IcdToCCSLong <- IcdToCCS[!is.na(IcdToCCS[,ccs_col]),] %>%
    group_by_("ID",ccs_col) %>%
    summarise(firstCaseDate = min(Date),
              endCaseDate = max(Date),
              period = endCaseDate - firstCaseDate,
              count = n())

  wrongFormat <- conversion$Error
  error_ICD <- anti_join(data.frame(ICD = IcdToCCS$ICD[is.na(IcdToCCS[,ccs_col])],stringsAsFactors = F), wrongFormat, "ICD")

  if(anyNA(IcdToCCS)){
    if(nrow(wrongFormat) > 0){
      message(paste0("wrong Format: ", unique(wrongFormat$ICD), sep = "\t\n"))
    }
    if(sum(is.na(IcdToCCS)) > nrow(wrongFormat)){
      message(paste0("wrong ICD version: ", unique(error_ICD$ICD), sep = "\t\n"))
      message("\n")
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
    warning('"wrong ICD version" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = F)
  }
  return(list(groupedDf = IcdToCCS,
              groupedData_Long = IcdToCCSLong))
}
