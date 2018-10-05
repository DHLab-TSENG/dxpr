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
#' @examples
#' DxDataFile <- data.frame(ID=c("A","A","A"),
#'                          ICD=c("6929","V433","I350"),
#'                          Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10")),
#'                          stringsAsFactors = FALSE)
#' groupIcdDxToCCS(DxDataFile, ID, ICD, Date, "2016-01-01", TRUE)
#'
groupIcdDxToCCS <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, isCCSCategoryDescription = TRUE){
  DxDataFile <- DxDataFile[, c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  Format <- ifelse(any(grepl("[.]", DxDataFile$ICD)), "Decimal", "Short")
  DxDataFile$ICD <- convertIcdDxDecimaltoShort(DxDataFile$ICD)$Short

  icd10 <- DxDataFile[DxDataFile$Date >= icd10usingDate,] %>% unique()
  icd9 <- DxDataFile[DxDataFile$Date < icd10usingDate,] %>% unique()

  icd9ToCCS <- left_join(icd9, select(ccsDxICD9, ICD, CCS_CATEGORY, CCS_CATEGORY_DESCRIPTION), by = "ICD")
  icd10ToCCS <- left_join(icd10, select(ccsDxICD10, ICD, CCS_CATEGORY, CCS_CATEGORY_DESCRIPTION), by = "ICD")
  CCS_combine <- rbind(icd9ToCCS, icd10ToCCS)

  CCS_combine_with_originalFile <- left_join(DxDataFile, CCS_combine, by = c("ID", "ICD", "Date"))

  if (isCCSCategoryDescription == T) {
    IcdToCCS <- CCS_combine_with_originalFile$CCS_CATEGORY_DESCRIPTION
  } else {
    IcdToCCS <- CCS_combine_with_originalFile$CCS_CATEGORY
  }
  WrongFormat <- convertIcdDxDecimaltoShort(DxDataFile$ICD)$Error
  error_ICD <- anti_join(data.frame(ICD = CCS_combine_with_originalFile$ICD[is.na(IcdToCCS)], stringsAsFactors= FALSE),
                         data.frame(ICD = WrongFormat, stringsAsFactors= FALSE), "ICD") %>% unique
  if(anyNA(IcdToCCS)){
    if(length(WrongFormat) > 0){
      message(paste0("wrong Format: ", unique(WrongFormat), sep = "\t\n"))
    }
    if(sum(is.na(IcdToCCS)) > length(WrongFormat)){
      if(Format == "Decimal"){
        message(paste0("warning ICD: ", convertIcdDxShortToDecimal(error_ICD$ICD)$Decimal, sep = "\t\n"))
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
