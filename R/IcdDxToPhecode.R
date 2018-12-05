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
#'
#' IcdDxToPhecode(testDxFile, ID, ICD, Date, "2015-10-01", FALSE)
#'
IcdDxToPhecode <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, isPhecodeDescription = TRUE){
  DxDataFile <- DxDataFile[ ,c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile$Date <- as.Date(DxDataFile$Date)
  DxDataFile <- DxDataFile %>% mutate(Number =  1:nrow(DxDataFile))
  icd9 <- DxDataFile[DxDataFile$Date < icd10usingDate,]
  conversion <- IcdDxShortToDecimal(icd9$ICD)
  icd9$Decimal <- conversion$Decimal

  if(isPhecodeDescription == T){
    phecodeCol <- "PheCodeDescription"
  }else{
    phecodeCol <- "PheCode"
  }
  IcdToPhecode <- left_join(DxDataFile, left_join(icd9, select_(phecode_icd9_2, "ICDD", phecodeCol), by = c("Decimal" = "ICDD")),
                            by = names(DxDataFile))

  IcdToPhecodeLong <- IcdToPhecode[!is.na(IcdToPhecode[,phecodeCol]),] %>%
    group_by_("ID",phecodeCol) %>%
    summarise(firstCaseDate = min(Date),
              endCaseDate = max(Date),
              period = endCaseDate - firstCaseDate,
              count = n())

  wrongFormat <- conversion$Error
  error_ICD <- anti_join(data.frame(ICD = IcdToPhecode$ICD[is.na(IcdToPhecode[,phecodeCol])],stringsAsFactors = F), wrongFormat, "ICD")

  if(anyNA(IcdToPhecode)){
    if(nrow(wrongFormat) > 0){
      message(paste0("wrong Format: ", unique(wrongFormat$ICD), sep = "\t\n"))
    }
    if(sum(is.na(IcdToPhecode)) > nrow(wrongFormat)){
      message(paste0("wrong ICD version: ", unique(error_ICD$ICD), sep = "\t\n"))
      message("\n")
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
    warning('"wrong ICD version" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = F)
  }
  return(list(groupedIcd = IcdToPhecode[,phecodeCol],
              groupedData_Long = IcdToPhecodeLong))
}
