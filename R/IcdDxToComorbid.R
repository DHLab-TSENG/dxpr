#' @rdname DxComorbid
#' @export
#'
IcdDxToComorbid <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, comorbidMethod, isDescription = FALSE){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(DxDataFile))]
  Conversion <- IcdDxDecimalToShort(DxDataFile,ICD,Date,icd10usingDate)
  DxDataFile[,Short := Conversion$ICD]

  comorbidMethod <- tolower(deparse(substitute(comorbidMethod)))
  if(comorbidMethod == "ahrq"){
    comorbidMap9 <- `icd9_ahrq`
    comorbidMap10 <- `icd10_ahrq`
  }else if(comorbidMethod == "charlson"){
    comorbidMap9 <- `icd9_charlson`
    comorbidMap10 <- `icd10_charlson`
  }else if(comorbidMethod == "elix"){
    comorbidMap9 <- `icd9_elix`
    comorbidMap10 <- `icd10_elix`
  }else{
    stop("'please enter AHRQ, Charlson or Elix for 'comorbidMethod'", call. = FALSE)
  }

  if (isDescription){
    com_col <- "Description"
  }else{
    com_col <- "Comorbidity"
  }

  allComorbid <- rbind(merge(DxDataFile[Date <icd10usingDate], comorbidMap9[,c("ICD",com_col),with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                       merge(DxDataFile[Date >=icd10usingDate], comorbidMap10[,c("ICD",com_col),with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  allComorbid <- allComorbid[order(Number),-"Number"]

  if(nrow(allComorbid[is.na(eval(parse(text = paste(com_col))))]) < nrow(allComorbid)){
    summarisedComorbid <- allComorbid[!is.na(eval(parse(text = paste(com_col)))),
                                      list(firstCaseDate = min(Date),
                                           endCaseDate = max(Date),
                                           count = .N),
                                      by = c("ID",com_col)][,period := (endCaseDate - firstCaseDate),][order(ID),]
    return(list(groupedDT = allComorbid,
                summarised_groupedDT = summarisedComorbid,
                Error = Conversion$Error))
  }else{
    return(list(groupedDT = allComorbid,
                Error = Conversion$Error))
  }
}
