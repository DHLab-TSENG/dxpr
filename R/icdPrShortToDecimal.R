#' @rdname prUniform
#' @export
#'
icdPrShortToDecimal<-function(prDataFile, icdColName, dateColName, icdVerColName = NULL, icd10usingDate = NULL){

  if(deparse(substitute(icdVerColName)) != "NULL"){
    dataCol <- c(deparse(substitute(icdColName)), deparse(substitute(dateColName)), deparse(substitute(icdVerColName)))
    prDataFile <- setDT(prDataFile)[,dataCol,with = FALSE]
    names(prDataFile) <- c("ICD", "Date", "Version")
  }else{
    dataCol <- c(deparse(substitute(icdColName)), deparse(substitute(dateColName)))
    prDataFile <- setDT(prDataFile)[,dataCol,with = FALSE]
    names(prDataFile) <- c("ICD", "Date")
  }

  prDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(prDataFile))]

  icd_Decimal <- prDataFile[grepl("[.]",prDataFile$ICD),]
  if(nrow(icd_Decimal) > 0){

    if(deparse(substitute(icdVerColName)) != "NULL"){
      icd9D <- merge(icd_Decimal[Version == 9], ICD9PrwithTwoFormat, by.x = "ICD", by.y = "Decimal", all.x = TRUE)
      icd10DNA <- merge(icd_Decimal[Version == 10], ICD9DxwithTwoFormat, by.x = "ICD", by.y = "Decimal", all.x = TRUE)
    }else{
      icd9D <- merge(icd_Decimal[Date < icd10usingDate], ICD9PrwithTwoFormat, by.x = "ICD", by.y = "Decimal", all.x = TRUE)
      icd10DNA <- merge(icd_Decimal[Date >= icd10usingDate], ICD9DxwithTwoFormat, by.x = "ICD", by.y = "Decimal", all.x = TRUE)
    }
    if(anyNA(icd9D)){icd9DNA <- icd9D[is.na(Short),]}
    if(exists("icd9DNA")){icd9DwrongFormat <- icd9DNA[, list(count = .N), by = ICD]}

    if(nrow(icd10DNA) > 0){
      icd10DwrongFormat <- icd10DNA[is.na(Short), list(count = .N), by = ICD]
      icd10DWrongVer <- icd10DNA[!is.na(Short), list(count = .N), by = ICD]
    }
    if(exists("icd9DwrongFormat") && nrow(icd9DwrongFormat) > 0 && exists("icd10DwrongFormat") && nrow(icd10DwrongFormat) > 0){
      wrongFormat_D <- rbind(icd9DNA[is.na(Short), -"Short"], icd10DNA[is.na(Short), -"Short"])
      wrongFormatMsg_D <- rbind(icd9DwrongFormat[, IcdVersionInFile := "ICD 9"], icd10DwrongFormat[, IcdVersionInFile := "ICD 10"])
    }else if(exists("icd9DwrongFormat") && nrow(icd9DwrongFormat) > 0){
      wrongFormat_D <- icd9DNA[is.na(Short), -"Short"]
      wrongFormatMsg_D <- icd9DwrongFormat[,IcdVersionInFile := "ICD 9"]
    }else if(exists("icd10DwrongFormat") && nrow(icd10DwrongFormat) > 0){
      wrongFormat_D <- icd10DNA[is.na(Short), -"Short"]
      wrongFormatMsg_D <- icd10DwrongFormat[,IcdVersionInFile := "ICD 10"]
    }

    if(exists("icd10DWrongVer") && nrow(icd10DWrongVer) > 0){
      wrongVersion_D <- icd10DNA[!is.na(Short), -"Short"]
      wrongVersionMsg_D <- icd10DWrongVer[, IcdVersionInFile := "ICD 10"]
    }
    DtoD <- icd9D[!is.na(Short),-"Short"]
  }else{
    DtoD <- icd_Decimal
  }

  icd_Short <- prDataFile[!icd_Decimal, on = "Number"]
  if(nrow(icd_Short) > 0){
    if(deparse(substitute(icdVerColName)) != "NULL"){
      icd9S <- merge(icd_Short[Version == 9], ICD9PrwithTwoFormat, by.x = "ICD", by.y = "Short", all.x = TRUE)
      icd10S <- merge(icd_Short[Version == 10], prICD10, by =  "ICD", all.x = TRUE)
    }else{
      icd9S <- merge(icd_Short[Date < icd10usingDate], ICD9PrwithTwoFormat, by.x = "ICD", by.y = "Short", all.x = TRUE)
      icd10S <- merge(icd_Short[Date >= icd10usingDate], prICD10, by =  "ICD", all.x = TRUE)
    }

    if(anyNA(icd9S)){icd9SNA <- merge(icd9S[is.na(Decimal),-"Decimal"], prICD10,by = "ICD",all.x = TRUE)}
    if(anyNA(icd10S)){icd10SNA <- merge(icd10S[is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"], ICD9PrwithTwoFormat,by.x = "ICD", by.y = "Short", all.x = TRUE)}

    if(exists("icd9SNA")){
      icd9SwrongFormat <- icd9SNA[is.na(ICD_DESCRIPTION), list(count = .N), by = ICD]
      icd9SWrongVer <- icd9SNA[!is.na(ICD_DESCRIPTION), list(count = .N), by = ICD]
    }
    if(exists("icd10SNA")){
      icd10SwrongFormat <- icd10SNA[is.na(Decimal), list(count = .N),by = ICD]
      icd10SWrongVer <- icd10SNA[!is.na(Decimal), list(count = .N),by = ICD]
    }
    if(exists("icd9SwrongFormat") && nrow(icd9SwrongFormat) > 0 && exists("icd10SwrongFormat") && nrow(icd10SwrongFormat) > 0){
      wrongFormat_S <- rbind(icd9SNA[is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"], icd10SNA[is.na(Decimal),-"Decimal"])
      wrongFormatMsg_S<- rbind(icd9SwrongFormat[, IcdVersionInFile := "ICD 9"],
                               icd10SwrongFormat[, IcdVersionInFile := "ICD 10"])
    }else if(exists("icd9SwrongFormat") && nrow(icd9SwrongFormat) > 0){
      wrongFormat_S <- icd9SNA[is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"]
      wrongFormatMsg_S <- icd9SwrongFormat[,IcdVersionInFile:="ICD 9"]
    }else if(exists("icd10SwrongFormat") && nrow(icd10SwrongFormat) > 0){
      wrongFormat_S <- icd10SNA[is.na(Decimal),-"Decimal"]
      wrongFormatMsg_S<- icd10SwrongFormat[,IcdVersionInFile:="ICD 10"]
    }
    if(exists("icd9SWrongVer") && nrow(icd9SWrongVer) > 0 && exists("icd10SWrongVer") && nrow(icd10SWrongVer) > 0){
      wrongVersion_S <- rbind(icd9SNA[!is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"], icd10SNA[!is.na(Decimal),-"Decimal"])
      wrongVersionMsg_S <- rbind(icd9SWrongVer[,IcdVersionInFile := "ICD 9"],
                                 icd10SWrongVer[,IcdVersionInFile := "ICD 10"])
    }else if(exists("icd9SWrongVer") && nrow(icd9SWrongVer) > 0){
      wrongVersion_S <- icd9SNA[!is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"]
      wrongVersionMsg_S <-icd9SWrongVer[,IcdVersionInFile := "ICD 9"]
    }else if(exists("icd10SWrongVer") && nrow(icd10SWrongVer) > 0){
      wrongVersion_S <- icd10SNA[!is.na(Decimal),-"Decimal"]
      wrongVersionMsg_S <- icd10SWrongVer[,IcdVersionInFile:="ICD 10"]
    }
    StoD_9 <- icd9S[!is.na(Decimal),-"ICD"]
    setnames(StoD_9,"Decimal","ICD")
    StoD <- rbind(StoD_9, icd10S[!is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"])
  }else{
    StoD <- icd_Short
  }
  if(exists("wrongFormat_D") && exists("wrongFormat_S")){
    allWrongFormat <- rbind(wrongFormat_D, wrongFormat_S)
    allWrongFormatMsg<- rbind(wrongFormatMsg_D,wrongFormatMsg_S)
  }else if(exists("wrongFormat_D")){
    allWrongFormat <- wrongFormat_D
    allWrongFormatMsg <- wrongFormatMsg_D
  }else if(exists("wrongFormat_S")){
    allWrongFormat <- wrongFormat_S
    allWrongFormatMsg<- wrongFormatMsg_S
  }
  if(exists("wrongVersion_D") && exists("wrongVersion_S")){
    allWrongVersion <- rbind(wrongVersion_D, wrongVersion_S)
    allWrongVersionMsg<- rbind(wrongVersionMsg_D, wrongVersionMsg_S)
  }else if(exists("wrongVersion_D")){
    allWrongVersion <- wrongVersion_D
    allWrongVersionMsg <- wrongVersionMsg_D
  }else if(exists("wrongVersion_S")){
    allWrongVersion <- wrongVersion_S
    allWrongVersionMsg<- wrongVersionMsg_S
  }

  if(exists("allWrongVersion") && exists("allWrongFormat")){
    allWrongICD <- rbind(allWrongFormat, allWrongVersion)
    allWrongICDMsg <- rbind(allWrongFormatMsg[,WrongType:="Wrong format"],
                            allWrongVersionMsg[,WrongType:="Wrong version"])
  }else if(exists("allWrongVersion")){
    allWrongICD <- allWrongVersion
    allWrongICDMsg <- allWrongVersionMsg[,WrongType:="Wrong version"]
  }else if(exists("allWrongFormat")){
    allWrongICD <- allWrongFormat
    allWrongICDMsg <- allWrongFormatMsg[,WrongType:="Wrong format"]
  }
  allDecimalFormat <- rbind(DtoD,StoD)

  if(nrow(allDecimalFormat) < nrow(prDataFile)){
    if(exists("allWrongFormat")){
      message(paste0("Wrong ICD format: total ",nrow(allWrongFormatMsg)," ICD codes (the number of occurrences is in brackets)"))
      allWrongFormatMsg <- allWrongFormatMsg[order(count,decreasing = TRUE),]
      message(head(allWrongFormatMsg[,list(wrongFormat= paste0(ICD," (",count,")","")),],10))
      message(("\t"))
    }
    if(exists("allWrongVersion")){
      message(paste0("Wrong ICD version: total ",nrow(allWrongVersionMsg)," ICD codes (the number of occurrences is in brackets)"))
      allWrongVersionMsg <- allWrongVersionMsg[order(count,decreasing = TRUE),]
      message(head(allWrongVersionMsg[,list(wrongFormat= paste0(ICD," (",count,")","")),], 10))
      message(("\t"))
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = FALSE)
    warning('"Wrong ICD format" means the ICD has wrong format', call. = FALSE)
    warning('"Wrong ICD version" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = FALSE)

    combine_with_error <- rbind(allWrongICD, allDecimalFormat)[order(Number),"ICD"]
    return(list(ICD = combine_with_error,
                Error = allWrongICDMsg[order(count,decreasing = TRUE),]))
  }else{
    return(list(ICD = allDecimalFormat[order(Number),"ICD"]))
  }
}
