
#' Get the PheWAS or description of ICD-9 diagnosis codes
#'
#' This can be used to group PheWAS or description based on ICD-9 codes in clinical diagnostic data.
#'
#' return PheWAS or description based on ICD-9-CM codes
#'
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @param isDescription PheWAS/ description for icd9. By default it is set to \code{True}.
#' @source ICD-9-PheWAS (version 1.2, 2015)
#' @source \url{https://phewascatalog.org/PheWASs}
#' @export
#' @examples
#' head(sampleDxFile)
#' PheWAS <- IcdDxToPheWAS(sampleDxFile, ID, ICD, Date, "2015-10-01", FALSE)
#' head(PheWAS)
#'
IcdDxToPheWAS <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, isDescription = TRUE){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  Conversion <- IcdDxShortToDecimal(DxDataFile,ICD,Date,icd10usingDate)
  DxDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(DxDataFile))]
  DxDataFile[,Decimal := Conversion$ICD]

  if(isDescription){
    PheWASCol <- "PheCodeDescription"
  }else{
    PheWASCol <- "PheCode"
  }
  allPheWAS <- rbind(merge(DxDataFile[Date < icd10usingDate,], phecode_icd9_2[,c(PheWASCol,"ICD"), with = FALSE], by.x ="Decimal", by.y = "ICD", all.x = TRUE),
                       merge(DxDataFile[Date >= icd10usingDate,], phecode_icd9_2[,c(PheWASCol,"ICD"), with = FALSE], by.x ="Decimal", by.y = "ICD", all.x = TRUE))
  allPheWAS <- allPheWAS[order(Number),-"Number"]

  if(nrow(allPheWAS[is.na(eval(parse(text = paste(PheWASCol))))]) < nrow(allPheWAS)){
    summarisedPheWAS <- allPheWAS[!is.na(eval(parse(text = paste(PheWASCol)))),
                                  list(firstCaseDate = min(Date),
                                       endCaseDate = max(Date),
                                       count = .N),
                                  by = c("ID",PheWASCol)][,period := (endCaseDate - firstCaseDate),]

    return(list(groupedDT = allPheWAS,
                summarised_groupedDT = summarisedPheWAS,
                Error = Conversion$Error))
  }else{
    return(list(groupedDT = allPheWAS,
                Error = Conversion$Error))
  }
}
