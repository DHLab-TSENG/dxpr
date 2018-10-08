if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "PcPrICD9",
  "PcPrICD10"))
#' Get the Procedure Class for ICD-9 and ICD-10 codes on procedures.
#'
#' 
#' return Procedure Class categories based on ICD-9 and ICD-10 codes
#'
#' @import dplyr
#' @importFrom stats complete.cases
#' @param PrDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A column for MemberID of PrDataFile
#' @param icdColName A column for ICD of PrDataFile
#' @param dateColName A column for Date of PrDataFile
#' @param icd10usingDate ICD-10 using date
#' @param isProcedureClassName  Procedure Class category/name for ICD-9 or ICD-10, default is True
#' @export
#' @examples
#' PrDataFile <- data.frame(ID=c("A","A","A","B"),
#'                          ICD=c("0101","8838","00870ZZ","00920ZZ"),
#'                          Date=as.Date(c("2013-03-31","2013-01-29","2016-03-10","2016-03-10")),
#'                          stringsAsFactors = FALSE)
#' groupIcdPrToProcedureClass(PrDataFile, ID, ICD, Date, "2016-01-01", TRUE)
#'
groupIcdPrToProcedureClass <- function(PrDataFile, idColName, icdColName, dateColName, icd10usingDate, isProcedureClassName = TRUE){
  PrDataFile <- PrDataFile[, c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))]
  Format <- ifelse(any(grepl("[.]", PrDataFile$ICD)), "Decimal", "Short")
  names(PrDataFile) <- c("ID", "ICD", "Date")
  PrDataFile$ICD <- convertIcdPrDecimaltoShort(PrDataFile$ICD)$Short
  
  icd9 <- PrDataFile[PrDataFile$Date < icd10usingDate,] %>% unique()
  icd10 <- PrDataFile[PrDataFile$Date >= icd10usingDate,] %>% unique()
  
  icd9ToPC <- left_join(icd9, PcPrICD9, by = "ICD")
  icd10ToPC <- left_join(icd10, PcPrICD10, by = "ICD")
  PC_combine <- rbind(icd9ToPC, icd10ToPC)
  
  PC_combine_with_originalFile <- left_join(PrDataFile, PC_combine, by = c("ID", "ICD", "Date"))
  
  if (isProcedureClassName == T) {
    IcdToPC <- PC_combine_with_originalFile$PROCEDURE_CLASS
  } else {
    IcdToPC <- PC_combine_with_originalFile$PROCEDURE_CLASS_NAME
  }
  WrongFormat <- convertIcdPrDecimaltoShort(PrDataFile$ICD)$Error
  error_ICD <- anti_join(data.frame(ICD = PC_combine_with_originalFile$ICD[is.na(IcdToPC)], stringsAsFactors= FALSE),
                         data.frame(ICD = WrongFormat, stringsAsFactors= FALSE), "ICD") %>% unique
  
  if(anyNA(IcdToPC)){
    if(length(WrongFormat) > 0){
      message(paste0("wrong Format: ", unique(WrongFormat), sep = "\t\n"))
    }
    if(sum(is.na(IcdToPC)) > length(WrongFormat)){
      if(Format == "Decimal"){
        message(paste0("warning ICD: ", convertIcdPrShortToDecimal(error_ICD$ICD)$Decimal, sep = "\t\n"))
      }else{
        message(paste0("warning ICD: ", error_ICD, sep = "\t\n"))
      }
      message("\n")
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"wrong Format" means the ICD has wrong format', call. = F)
    warning('"warning ICD" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = F)
  }
  IcdToPC
}
