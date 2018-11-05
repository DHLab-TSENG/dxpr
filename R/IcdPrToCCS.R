if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "ccsPrICD9",
  "ccsPrICD10"))
#' Get the Clinical Classifications Software (CCS) categories and description for ICD-9 and ICD-10 codes on procedures.
#'
#' Clinical Classifications Software (CCS) for ICD-9 and ICD-10 procedure codes in clinical diagnostic data is a procedure categorization scheme.
#'
#' return Clinical Classifications Software (CCS) categories or description based on ICD-9 and ICD-10 codes
#'
#' @import dplyr
#' @importFrom stats complete.cases
#' @param PrDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A column for MemberID of PrDataFile
#' @param icdColName A column for ICD of PrDataFile
#' @param dateColName A column for Date of PrDataFile
#' @param icd10usingDate ICD-10 using date
#' @param isCCSCategoryDescription  Clinical Classifications Software (CCS) single level categories/description for ICD-9 or ICD-10, default is True
#' @export
#' @source ICD-9-PCS CCS (2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source ICD-10-PCS CCS (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_pr_icd10pcs_2019_1.zip}
#' @examples
#' PrDataFile <- data.frame(ID=c("A","A","A","B"),
#'                          ICD=c("0101","8838","00870ZZ","00920ZZ"),
#'                          Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10","2016-03-10")),
#'                          stringsAsFactors = FALSE)
#' IcdPrToCCS(PrDataFile, ID, ICD, Date, "2016-01-01", TRUE)
#'
IcdPrToCCS <- function(PrDataFile, idColName, icdColName, dateColName, icd10usingDate, isCCSCategoryDescription = TRUE){
  PrDataFile <- PrDataFile[, c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(PrDataFile) <- c("ID", "ICD", "Date")
  PrDataFile <- PrDataFile %>% mutate(Number =  1:nrow(PrDataFile))
  Conversion <- IcdPrDecimaltoShort(PrDataFile$ICD)
  PrDataFile$Short <- Conversion$Short

  icd9ToCCS <- left_join(PrDataFile[as.Date(PrDataFile$Date) < icd10usingDate,],
                         select(ccsPrICD9, ICD, CCS_CATEGORY, CCS_CATEGORY_DESCRIPTION), by = c("Short"="ICD"))
  icd10ToCCS <- left_join(PrDataFile[as.Date(PrDataFile$Date) >= icd10usingDate,],
                          select(ccsPrICD10, ICD, CCS_CATEGORY, CCS_CATEGORY_DESCRIPTION), by = c("Short"="ICD"))
  CCS_combine <- rbind(icd9ToCCS, icd10ToCCS)

  if (isCCSCategoryDescription == T) {
    IcdToCCS <- CCS_combine$CCS_CATEGORY_DESCRIPTION
  } else {
    IcdToCCS <- CCS_combine$CCS_CATEGORY
  }
  WrongFormat <- Conversion$Error
  error_ICD <- anti_join(data.frame(ICD = CCS_combine$ICD[is.na(IcdToCCS)], stringsAsFactors= FALSE),WrongFormat, "ICD")
  if(anyNA(IcdToCCS)){
    if(nrow(WrongFormat) > 0){
      message(paste0("wrong Format: ", unique(WrongFormat$ICD), sep = "\t\n"))
    }
    if(sum(is.na(IcdToCCS)) > nrow(WrongFormat)){
      message(paste0("wrong ICD version: ", unique(error_ICD$ICD), sep = "\t\n"))
      message("\n")
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
    warning('"wrong ICD version" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = F)
  }
  IcdToCCS
}
