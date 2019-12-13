#' @rdname dxComorbid
#' @export
#'
icdDxToComorbid <- function(dxDataFile, idColName, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL, comorbidMethod, isDescription = FALSE){
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

  dxDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(dxDataFile))]

  if(deparse(substitute(icdVerColName)) != "NULL"){
    Conversion <- icdDxDecimalToShort(dxDataFile, ICD, Date, icdVerColName = Version, icd10usingDate = NULL)
  }else{
    Conversion <- icdDxDecimalToShort(dxDataFile, ICD, Date, icdVerColName = NULL, icd10usingDate = icd10usingDate)
  }

  dxDataFile[,Short := Conversion$ICD]

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

  if (deparse(substitute(icdVerColName)) != "NULL"){
    allComorbid <- rbind(merge(dxDataFile[Version == 9], comorbidMap9[,c("ICD",com_col),with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE, allow.cartesian=TRUE),
                         merge(dxDataFile[Version == 10], comorbidMap10[,c("ICD",com_col),with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE, allow.cartesian=TRUE))
  }else{
    allComorbid <- rbind(merge(dxDataFile[Date <icd10usingDate], comorbidMap9[,c("ICD",com_col),with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE, allow.cartesian=TRUE),
                         merge(dxDataFile[Date >=icd10usingDate], comorbidMap10[,c("ICD",com_col),with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE, allow.cartesian=TRUE))
  }
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

