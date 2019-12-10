#' @rdname dxPheWAS
#' @export
icdDxToPheWAS <- function(dxDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, isDescription = TRUE){
  dxDataFile <- as.data.table(dxDataFile)
  if(deparse(substitute(icdVerColName)) != "NULL"){
    dataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)), deparse(substitute(icdVerColName)))
    dxDataFile <- dxDataFile[,dataCol,with = FALSE]
    names(dxDataFile) <- c("ID", "ICD", "Date", "Version")
  }else{
    dataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
    dxDataFile <- dxDataFile[,dataCol,with = FALSE]
    names(dxDataFile) <- c("ID", "ICD", "Date")
  }
  if(deparse(substitute(icdVerColName)) != "NULL"){
    Conversion <- icdDxShortToDecimal(dxDataFile, ICD, Date, icdVerColName = Version, icd10usingDate = NULL)
  }else{
    Conversion <- icdDxShortToDecimal(dxDataFile, ICD, Date, icdVerColName = NULL, icd10usingDate = icd10usingDate)
  }

  dxDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(dxDataFile))]
  dxDataFile[,Decimal := Conversion$ICD]

  if(isDescription){
    PheWASCol <- "PheCodeDescription"
  }else{
    PheWASCol <- "PheCode"
  }

  if (deparse(substitute(icdVerColName)) != "NULL"){
    allPheWAS <- rbind(merge(dxDataFile[Version == 9,], phecode_icd9_2[,c(PheWASCol,"ICD"), with = FALSE], by.x ="Decimal", by.y = "ICD", all.x = TRUE),
                       merge(dxDataFile[Version == 10,], phecode_icd9_2[,c(PheWASCol,"ICD"), with = FALSE], by.x ="Decimal", by.y = "ICD", all.x = TRUE))
  }else{
    allPheWAS <- rbind(merge(dxDataFile[Date < icd10usingDate,], phecode_icd9_2[,c(PheWASCol,"ICD"), with = FALSE], by.x ="Decimal", by.y = "ICD", all.x = TRUE),
                       merge(dxDataFile[Date >= icd10usingDate,], phecode_icd9_2[,c(PheWASCol,"ICD"), with = FALSE], by.x ="Decimal", by.y = "ICD", all.x = TRUE))
  }
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
