if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "pcICD9",
  "pcICD10"))
#' Get the Procedure Class for ICD-9 and ICD-10 codes on procedures.
#'
#'
#' return Procedure Class categories based on ICD-9 and ICD-10 codes
#'
#' @import data.table
#' @importFrom stats complete.cases
#' @param PrDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A column for MemberID of PrDataFile
#' @param icdColName A column for ICD of PrDataFile
#' @param dateColName A column for Date of PrDataFile
#' @param icd10usingDate ICD-10 using date
#' @param isProcedureClassName  Procedure Class category/name for ICD-9 or ICD-10, default is True
#' @export
#' @source ICD-9-Procedure Class (2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/procedure/pc2015.csv}
#' @source ICD-10-Procedure Class (2018)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/procedureicd10/procedure_icd10.jsp}
#' @examples
#' head(samplePrFile)
#' IcdPrToProcedureClass(samplePrFile, ID, ICD, Date, "2015-10-01", TRUE)
#'
IcdPrToProcedureClass <- function(PrDataFile, idColName, icdColName, dateColName, icd10usingDate, isProcedureClassName = TRUE){
  PrDataFile <- as.data.table(PrDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  PrDataFile <- PrDataFile[,DataCol,with = FALSE]
  names(PrDataFile) <- c("ID","ICD", "Date")
  PrDataFile[,"Date"] <- as.Date(PrDataFile[,Date])
  PrDataFile[,Number:=1:nrow(PrDataFile)]
  PrDataFile[,Short:=IcdPrDecimalToShort(PrDataFile,ICD,Date,icd10usingDate)$ICD]

  if (isProcedureClassName == T) {
    PC_col <- "PROCEDURE_CLASS"
  } else {
    PC_col <- "PROCEDURE_CLASS_NAME"
  }
  IcdToPC <- rbind(merge(PrDataFile[Date < icd10usingDate],pcICD9[,c("ICD", PC_col), with = F],by.x ="Short",by.y = "ICD",all.x = T),
                   merge(PrDataFile[Date >= icd10usingDate],pcICD10[,c("ICD", PC_col), with = F],by.x ="Short",by.y = "ICD",all.x = T))
  IcdToPC <- IcdToPC[order(Number)][,eval(parse(text = paste(PC_col)))]
  IcdToPC
}
