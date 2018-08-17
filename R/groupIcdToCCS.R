#' Get the Clinical Classifications Software (CCS) categories and description for ICD-9 and ICD-10 codes on diagnoses.
#'
#' Clinical Classifications Software (CCS) for ICD-9 and ICD-10 diagnosis codes in clinical diagnostic data is a diagnosis categorization scheme.
#'
#' return Clinical Classifications Software (CCS) categories or description based on ICD-9 and ICD-10 codes
#'
#' @import icd
#' @import dplyr
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate ICD-10 using date
#' @param isCCSCategoryDescription  Clinical Classifications Software (CCS) single level categories/ description for ICD-9 or ICD-10, default is True
#' @export
#' @examples
#' DxDataFile <- data.frame(ID=c("A","A","A"),
#'                          ICD=c("6929","V433","I350"),
#'                          Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10")),
#'                          stringsAsFactors = FALSE)
#' groupIcdToCCS (DxDataFile, ID, ICD, Date, "2016-01-01", TRUE)
#'
groupIcdToCCS <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, isCCSCategoryDescription=TRUE){
  DxDataFile <- DxDataFile[, c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile$ICD <- convertIcdDecimaltoShort(DxDataFile$ICD)

  icd10 <- DxDataFile[DxDataFile$Date >= icd10usingDate, "ICD"]
  icd9 <- DxDataFile[DxDataFile$Date < icd10usingDate, "ICD"]
  icd9 <- left_join(data.frame(ICD = icd9, stringsAsFactors = F), select(ccsDxICD9, ICD,CCS_CATEGORY, CCS_CATEGORY_DESCRIPTION), by="ICD") %>% unique()
  icd10 <- left_join(data.frame(ICD = icd10, stringsAsFactors = F), select(ccsDxICD10, ICD, CCS_CATEGORY, CCS_CATEGORY_DESCRIPTION), by="ICD") %>% unique()

  DxDataFile_combine <- full_join(icd9, icd10, by = c("ICD","CCS_CATEGORY", "CCS_CATEGORY_DESCRIPTION"))
  DxDataFile_combine<-DxDataFile_combine[complete.cases(DxDataFile_combine),]
  DxDataFile_combine_with_originalFile <- left_join(DxDataFile, DxDataFile_combine, by="ICD")

  if (isCCSCategoryDescription == T) {
    DxDataFile_combine_with_originalFile <- DxDataFile_combine_with_originalFile$CCS_CATEGORY_DESCRIPTION
  } else {
    DxDataFile_combine_with_originalFile <- DxDataFile_combine_with_originalFile$CCS_CATEGORY
  }
  errorID<-is.na(DxDataFile_combine_with_originalFile[is.na(DxDataFile_combine_with_originalFile)])

  if(sum(errorID)>=1){
    warning("NA means icd code does not match the format.",call. = F)
  }
  DxDataFile_combine_with_originalFile
}
