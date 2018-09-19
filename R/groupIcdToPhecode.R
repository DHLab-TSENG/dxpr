if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "ICD",
  "ICDD",
  "PheCode",
  "PheCodeDescription",
  "phecode_icd9_2"))
#' Get the Phecode or description of ICD-9 diagnosis codes
#'
#' This can be used to group Phecode or description based on ICD-9 codes in clinical diagnostic data.
#'
#' return Phecode or description based on ICD-9-CM codes
#'
#' @import dplyr
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param isPhecodeDescription Phecode/ description for icd9, default is True
#' @source \url{https://phewascatalog.org/phecodes}
#' @export
#' @examples
#' DxDataFile <- data.frame(ID = c("A", "A", "A"),
#'                          ICD = c("6929", "V433", "I350"),
#'                          Date = as.Date(c("2013-03-31", "2013-01-29", "2016-03-10")),
#'                          stringsAsFactors = FALSE)
#' groupIcdToPhecode(DxDataFile, ID, ICD, Date, "2016-01-01", FALSE)
#'
groupIcdToPhecode <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, isPhecodeDescription = TRUE){
  DxDataFile <- DxDataFile[ ,c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile$ICDD <- convertIcdShortToDecimal(DxDataFile$ICD)$Decimal

  icd9 <- DxDataFile[DxDataFile$Date < icd10usingDate,]

  icd9ToPhecode <- left_join(icd9, select(phecode_icd9_2, ICDD, PheCode, PheCodeDescription),by = "ICDD") %>% unique()
  icd9ToPhecode_with_originalFile <- left_join(DxDataFile, icd9ToPhecode, by = c("ID", "ICD", "ICDD", "Date"))

  if(isPhecodeDescription == T){
    IcdToPhecode <- icd9ToPhecode_with_originalFile$PheCodeDescription
  }else{
    IcdToPhecode <- icd9ToPhecode_with_originalFile$PheCode
  }

  if(anyNA(IcdToPhecode)){
    message(paste0("warning ICD: ", unique(icd9ToPhecode_with_originalFile$ICD[is.na(IcdToPhecode)]), sep = "\t\n"))
    warning('The ICD mentioned above matches to "NA" due to the format or other issues (phecode does not have icd10).', call. = F)
  }
  IcdToPhecode
}
