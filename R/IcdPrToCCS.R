if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "ccsPrICD9",
  "ccsPrICD10"))
#' Get the Clinical Classifications Software (CCS) categories and description for ICD-9 and ICD-10 codes on procedures.
#'
#' Clinical Classifications Software (CCS) for ICD-9 and ICD-10 procedure codes in clinical diagnostic data is a procedure categorization scheme.
#'
#' return Clinical Classifications Software (CCS) categories or description based on ICD-9 and ICD-10 codes
#'
#' @import data.table
#' @importFrom stats complete.cases
#' @param PrDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A column for MemberID of PrDataFile
#' @param icdColName A column for ICD of PrDataFile
#' @param dateColName A column for Date of PrDataFile
#' @param icd10usingDate ICD-10 using date
#' @param isDescription  Clinical Classifications Software (CCS) single level categories/description for ICD-9 or ICD-10. By default it is set to \code{True}.
#' @export
#' @source ICD-9-PCS CCS (2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source ICD-10-PCS CCS (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_pr_icd10pcs_2019_1.zip}
#' @examples
#' head(samplePrFile)
#' IcdPrToCCS(samplePrFile, ID, ICD, Date, "2015-10-01", TRUE)
#'
IcdPrToCCS <- function(PrDataFile, idColName, icdColName, dateColName, icd10usingDate, isDescription = TRUE){
  PrDataFile <- as.data.table(PrDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  PrDataFile <- PrDataFile[,DataCol,with = FALSE]
  names(PrDataFile) <- c("ID","ICD", "Date")
  Conversion <- IcdPrDecimalToShort(PrDataFile,ICD,Date,icd10usingDate)
  PrDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(PrDataFile))]
  PrDataFile[,Short := Conversion$ICD]


  if (isDescription == T) {
    ccs_col <- "CCS_CATEGORY_DESCRIPTION"
  } else {
    ccs_col <- "CCS_CATEGORY"
  }
  IcdToCCS <- rbind(merge(PrDataFile[Date <icd10usingDate],ccsPrICD9[,c("ICD",ccs_col), with = F],by.x ="Short",by.y = "ICD",all.x = T),
                    merge(PrDataFile[Date >=icd10usingDate],ccsPrICD10[,c("ICD",ccs_col), with = F],by.x ="Short",by.y = "ICD",all.x = T))
  IcdToCCS <- IcdToCCS[order(Number), -"Number"]

  return(list(groupedDT = IcdToCCS,
              Error = Conversion$Error))
}


