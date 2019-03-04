if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "ccsDxICD9",
  "CCS_CATEGORY",
  "CCS_CATEGORY_DESCRIPTION",
  "icd10usingDate",
  "ccsDxICD10",
  "isCCSCategoryDescription"))
#' Get the Clinical Classifications Software (CCS) categories and description for ICD-9 and ICD-10 codes on diagnoses.
#'
#' Clinical Classifications Software (CCS) for ICD-9 and ICD-10 diagnosis codes in clinical diagnostic data is a diagnosis categorization scheme.
#'
#' return Clinical Classifications Software (CCS) categories or description based on ICD-9 and ICD-10 codes
#'
#' @import data.table
#' @importFrom stats complete.cases
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID", "ICD", and "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate ICD-10 using date
#' @param isCCSCategoryDescription  Clinical Classifications Software (CCS) single level categories/description for ICD-9 or ICD-10. By default it is set to \code{True}.
#' @export
#' @source ICD-9-CM CCS (2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip}
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip}
#' @source ICD-10-CM CCS (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_dx_icd10cm_2019_1.zip}
#' @examples
#' head(sampleDxFile)
#' IcdDxToCCS(sampleDxFile, ID, ICD, Date, "2015-10-01", TRUE)
#'
IcdDxToCCS <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, isCCSCategoryDescription = T){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])
  DxDataFile[,Number:=1:nrow(DxDataFile)]
  Conversion <- IcdDxDecimalToShort(DxDataFile,ICD,Date,icd10usingDate)
  DxDataFile[,Short:= Conversion$ICD]

  if (isCCSCategoryDescription == T) {
    ccs_col <- "CCS_CATEGORY_DESCRIPTION"
  }else {
    ccs_col <- "CCS_CATEGORY"
  }
  IcdToCCS <- rbind(merge(DxDataFile[Date <icd10usingDate],ccsDxICD9[,c("ICD",ccs_col), with = F],by.x ="Short",by.y = "ICD",all.x = T),
                    merge(DxDataFile[Date >=icd10usingDate],ccsDxICD10[,c("ICD",ccs_col), with = F],by.x ="Short",by.y = "ICD",all.x = T))
  IcdToCCS <- IcdToCCS[order(Number),-"Number"]
  IcdToCCSLong <- IcdToCCS[!is.na(eval(parse(text = paste(ccs_col)))),
                           list(firstCaseDate = min(Date),
                                endCaseDate = max(Date),
                                count = .N),
                           by = c("ID",ccs_col)][,period := (endCaseDate - firstCaseDate),]

  return(list(groupedDf = IcdToCCS,
              groupedData_Long = IcdToCCSLong,
              Error = Conversion$Error))
}
