#' @rdname DxPheWAS
#' @export
IcdDxToPheWAS <- function(DxDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, isDescription = TRUE){
  DxDataFile <- as.data.table(DxDataFile)
  if(deparse(substitute(icdVerColName)) != "NULL"){
    DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)), deparse(substitute(icdVerColName)))
    DxDataFile <- DxDataFile[,DataCol,with = FALSE]
    names(DxDataFile) <- c("ID", "ICD", "Date", "Version")
  }else{
    DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
    DxDataFile <- DxDataFile[,DataCol,with = FALSE]
    names(DxDataFile) <- c("ID", "ICD", "Date")
  }
  if(deparse(substitute(icdVerColName)) != "NULL"){
    Conversion <- IcdDxShortToDecimal(DxDataFile, ICD, Date, icdVerColName = Version, icd10usingDate = NULL)
  }else{
    Conversion <- IcdDxShortToDecimal(DxDataFile, ICD, Date, icdVerColName = NULL, icd10usingDate = icd10usingDate)
  }

  DxDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(DxDataFile))]
  DxDataFile[,Decimal := Conversion$ICD]

  if(isDescription){
    PheWASCol <- "PheCodeDescription"
  }else{
    PheWASCol <- "PheCode"
  }

  if (deparse(substitute(icdVerColName)) != "NULL"){
    allPheWAS <- rbind(merge(DxDataFile[Version == 9,], phecode_icd9_2[,c(PheWASCol,"ICD"), with = FALSE], by.x ="Decimal", by.y = "ICD", all.x = TRUE),
                       merge(DxDataFile[Version == 10,], phecode_icd9_2[,c(PheWASCol,"ICD"), with = FALSE], by.x ="Decimal", by.y = "ICD", all.x = TRUE))
  }else{
    allPheWAS <- rbind(merge(DxDataFile[Date < icd10usingDate,], phecode_icd9_2[,c(PheWASCol,"ICD"), with = FALSE], by.x ="Decimal", by.y = "ICD", all.x = TRUE),
                       merge(DxDataFile[Date >= icd10usingDate,], phecode_icd9_2[,c(PheWASCol,"ICD"), with = FALSE], by.x ="Decimal", by.y = "ICD", all.x = TRUE))
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
