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
#' @source ICD-9-Phecode (version 1.2, 2015)
#' @source \url{https://phewascatalog.org/phecodes}
#' @export
#' @examples
#' DxDataFile <- data.frame(ID = c("A", "A", "A"),
#'                          ICD = c("6929", "V433", "I350"),
#'                          Date = as.Date(c("2013-03-31", "2013-01-29", "2016-03-10")),
#'                          stringsAsFactors = FALSE)
#' IcdDxToPhecode(DxDataFile, ID, ICD, Date, "2016-01-01", FALSE)
#'
IcdDxToPhecode <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, isPhecodeDescription = TRUE){
  DxDataFile <- DxDataFile[ ,c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile <- DxDataFile %>% mutate(Number =  1:nrow(DxDataFile))

  icd9 <- DxDataFile[as.Date(DxDataFile$Date) < icd10usingDate,]
  Conversion <- IcdDxShortToDecimal(icd9$ICD)
  icd9$ICDD <- Conversion$Decimal

  Phecode_combine <- full_join(DxDataFile, left_join(icd9, select(phecode_icd9_2, ICDD, PheCode, PheCodeDescription), "ICDD"),
                               by = c("ID", "ICD", "Date", "Number"))

  if(isPhecodeDescription == T){
    IcdToPhecode <- Phecode_combine$PheCodeDescription
  }else{
    IcdToPhecode <- Phecode_combine$PheCode
  }

  WrongFormat <- Conversion$Error
  error_ICD <- anti_join(data.frame(ICD = DxDataFile$ICD[is.na(IcdToPhecode)], stringsAsFactors= FALSE), WrongFormat, "ICD")
  if(anyNA(IcdToPhecode)){
    if(nrow(WrongFormat) > 0){
      message(paste0("wrong Format: ", unique(WrongFormat$ICD), sep = "\t\n"))
    }
    if(sum(is.na(IcdToPhecode)) > nrow(WrongFormat)){
      message(paste0("wrong ICD version: ", unique(error_ICD$ICD), sep = "\t\n"))
      message("\n")
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
    warning('"wrong ICD version" means the ICD classify to wrong ICD version (phecode does not have icd10), or the icd does not have phecode ', call. = F)
  }
  IcdToPhecode
}
