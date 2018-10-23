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
  Format <- ifelse(any(grepl("[.]", PrDataFile$ICD)), "Decimal", "Short")
  names(PrDataFile) <- c("ID", "ICD", "Date")
  PrDataFile$ICD <- IcdPrDecimaltoShort(PrDataFile$ICD)$Short

  icd10 <- PrDataFile[PrDataFile$Date >= icd10usingDate,] %>% unique()
  icd9 <- PrDataFile[PrDataFile$Date < icd10usingDate,] %>% unique()

  icd9ToCCS <- left_join(icd9, select(ccsPrICD9, ICD, CCS_CATEGORY, CCS_CATEGORY_DESCRIPTION), by = "ICD")
  icd10ToCCS <- left_join(icd10, select(ccsPrICD10, ICD, CCS_CATEGORY, CCS_CATEGORY_DESCRIPTION), by = "ICD")
  CCS_combine <- rbind(icd9ToCCS, icd10ToCCS)

  CCS_combine_with_originalFile <- left_join(PrDataFile, CCS_combine, by = c("ID", "ICD", "Date"))

  if (isCCSCategoryDescription == T) {
    IcdToCCS <- CCS_combine_with_originalFile$CCS_CATEGORY_DESCRIPTION
  } else {
    IcdToCCS <- CCS_combine_with_originalFile$CCS_CATEGORY
  }
  WrongFormat <- IcdPrDecimaltoShort(PrDataFile$ICD)$Error
  error_ICD <- anti_join(data.frame(ICD = CCS_combine_with_originalFile$ICD[is.na(IcdToCCS)], stringsAsFactors= FALSE),
                         data.frame(ICD = WrongFormat, stringsAsFactors= FALSE), "ICD") %>% unique
  if(anyNA(IcdToCCS)){
    if(length(WrongFormat) > 0){
      message(paste0("wrong Format: ", unique(WrongFormat), sep = "\t\n"))
    }
    if(sum(is.na(IcdToCCS)) > length(WrongFormat)){
      if(Format == "Decimal"){
        message(paste0("warning ICD: ", IcdPrShortToDecimal(error_ICD$ICD)$Decimal, sep = "\t\n"))
      }else{
        message(paste0("warning ICD: ", error_ICD, sep = "\t\n"))
      }
      message("\n")
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
    warning('"warning ICD" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = F)
  }
  IcdToCCS
}
