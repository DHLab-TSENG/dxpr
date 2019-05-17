
#' Get the Phecode or description of ICD-9 diagnosis codes
#'
#' This can be used to group Phecode or description based on ICD-9 codes in clinical diagnostic data.
#'
#' return Phecode or description based on ICD-9-CM codes
#'
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param isDescription Phecode/ description for icd9. By default it is set to \code{True}.
#' @source ICD-9-Phecode (version 1.2, 2015)
#' @source \url{https://phewascatalog.org/phecodes}
#' @export
#' @examples
#' head(sampleDxFile)
#' IcdDxToPhecode(sampleDxFile, ID, ICD, Date, "2015-10-01", FALSE)
#'
IcdDxToPhecode <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, isDescription = TRUE){
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  Conversion <- IcdDxShortToDecimal(DxDataFile,ICD,Date,icd10usingDate)
  DxDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(DxDataFile))]
  DxDataFile[,ICDD := Conversion$ICD]

  if(isDescription == T){
    phecodeCol <- "PheCodeDescription"
  }else{
    phecodeCol <- "PheCode"
  }
  IcdToPhecode <- rbind(merge(DxDataFile[Date < icd10usingDate,], phecode_icd9_2[,c(phecodeCol,"ICDD"), with = F], by = "ICDD", all.x = T),
                        merge(DxDataFile[Date >= icd10usingDate,], phecode_icd9_2[,c(phecodeCol,"ICDD"), with = F], by = "ICDD", all.x = T))
  IcdToPhecode <- IcdToPhecode[order(Number),-"Number"]
  IcdToPhecodeLong <- IcdToPhecode[!is.na(eval(parse(text = paste(phecodeCol)))),
                                   list(firstCaseDate = min(Date),
                                        endCaseDate = max(Date),
                                        count = .N),
                                   by = c("ID",phecodeCol)][,period := (endCaseDate - firstCaseDate),]

  return(list(groupedDT = IcdToPhecode,
              summarised_groupedDT = IcdToPhecodeLong,
              Error = Conversion$Error))
}
