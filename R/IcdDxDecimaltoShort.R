
#' Convert ICD Codes From Decimal To Short Forms
#'
#' Convert codes between short and decimal forms
#'
#' @import data.table
#' @importFrom utils head
#' @param DxDataFile A file of clinical diagnostic data with at least 2 columns: "ICD", "Date"
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Quality-Initiatives-Patient-Assessment-Instruments/HospitalQualityInits/Downloads/HospitalAppendix_F.pdf}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-CM.html}
#' @export
#' @examples
#' head(sampleDxFile)
#' IcdDxDecimalToShort(sampleDxFile,ICD,Date, "2015/10/01")
#'
IcdDxDecimalToShort<-function(DxDataFile, icdColName, dateColName, icd10usingDate){
  DataCol <- c(deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])
  DxDataFile[,Number:=1:nrow(DxDataFile)]

  icd_Decimal <- DxDataFile[grepl("[.]",DxDataFile$ICD),]
  icd9D <- merge(icd_Decimal[Date < icd10usingDate], ICD9DxwithTwoFormat, by.x = "ICD", by.y = "Decimal", all.x = T)
  icd10D <- merge(icd_Decimal[Date >= icd10usingDate], ICD10DxwithTwoFormat, by.x = "ICD", by.y = "Decimal", all.x = T)

  if(nrow(icd9D[is.na(Short),-"Short"]) > 0 | nrow(icd10D[is.na(Short),-"Short"])>0){
    icd9DNA <- merge(icd9D[is.na(Short),-"Short"],ICD10DxwithTwoFormat,by.x = "ICD",by.y = "Decimal",all.x = T)
    icd10DNA <- merge(icd10D[is.na(Short),-"Short"],ICD9DxwithTwoFormat,by.x = "ICD",by.y = "Decimal",all.x = T)

    icd9DwrongFormat <- icd9DNA[is.na(Short), list(count = .N),by = ICD]
    icd10DwrongFormat <- icd10DNA[is.na(Short), list(count = .N),by = ICD]
    icd9DWrongVer <- icd9DNA[!is.na(Short), list(count = .N),by = ICD]
    icd10DWrongVer <- icd10DNA[!is.na(Short), list(count = .N),by = ICD]

    if(nrow(icd9DwrongFormat) > 0){
      wrongFormatMsg_D <- icd9DwrongFormat[,IcdVersionInFile:="ICD 9"]
      wrongFormat_D <- icd9DNA[is.na(Short),-"Short"]
      if(nrow(icd10DwrongFormat) > 0){
        icd10DwrongFormat <- icd10DwrongFormat[,IcdVersionInFile:="ICD 10"]
        wrongFormatMsg_D <- rbind(wrongFormatMsg_D,icd10DwrongFormat)
        wrongFormat_D <- rbind(wrongFormat_D, icd10DNA[is.na(Short),-"Short"])
      }
    }else if(nrow(icd10DwrongFormat) > 0){
      wrongFormatMsg_D <- icd10DwrongFormat[,IcdVersionInFile:="ICD 10"]
      wrongFormat_D <- icd10DNA[is.na(Short),-"Short"]
    }else{
      wrongFormatMsg_D <- NA
      wrongFormat_D <- NA
    }

    if(nrow(icd9DWrongVer) > 0){
      wrongVersionMsg_D <- icd9DWrongVer[,IcdVersionInFile:="ICD 9"]
      wrongVersion_D <- icd9DNA[!is.na(Short),-"Short"]
      if(nrow(icd10DWrongVer) > 0){
        icd10DWrongVer <- icd10DWrongVer[,IcdVersionInFile:="ICD 10"]
        wrongVersionMsg_D <- rbind(wrongVersionMsg_D,icd10DWrongVer)
        wrongVersion_D <- rbind(wrongVersion_D,icd10DNA[!is.na(Short),-"Short"])
      }
    }else if(nrow(icd10DWrongVer) > 0){
      wrongVersionMsg_D <- icd10DWrongVer[,IcdVersionInFile:="ICD 10"]
      wrongVersion_D <- icd10DNA[!is.na(Short),-"Short"]
    }else{
      wrongVersionMsg_D <-NA
      wrongVersion_D <- NA
    }
  }else{
    wrongFormatMsg_D <- NA
    wrongFormat_D <- NA
    wrongVersionMsg_D <-NA
    wrongVersion_D <- NA
  }
  icd_Short <- DxDataFile[!icd_Decimal, on = "Number"]
  icd9S <- merge(icd_Short[Date < icd10usingDate], ICD9DxwithTwoFormat, by.x = "ICD", by.y = "Short", all.x = T)
  icd10S <- merge(icd_Short[Date >= icd10usingDate], ICD10DxwithTwoFormat, by.x = "ICD", by.y = "Short", all.x = T)

  if(nrow(icd9S[is.na(Decimal),-"Decimal"]) > 0 | nrow(icd10S[is.na(Decimal),-"Decimal"]) > 0){
    icd9SNA <- merge(icd9S[is.na(Decimal),-"Decimal"], ICD10DxwithTwoFormat,by.x = "ICD",by.y = "Short",all.x = T)
    icd10SNA <- merge(icd10S[is.na(Decimal),-"Decimal"], ICD9DxwithTwoFormat,by.x = "ICD",by.y = "Short",all.x = T)
    icd9SwrongFormat <- icd9SNA[is.na(Decimal), list(count = .N),by = ICD]
    icd10SwrongFormat <- icd10SNA[is.na(Decimal), list(count = .N),by = ICD]
    icd9SWrongVer <- icd9SNA[!is.na(Decimal), list(count = .N),by = ICD]
    icd10SWrongVer <- icd10SNA[!is.na(Decimal), list(count = .N),by = ICD]

    if(nrow(icd9SwrongFormat) > 0){
      wrongFormatMsg_S <- icd9SwrongFormat[,IcdVersionInFile:="ICD 9"]
      wrongFormat_S <- icd9SNA[is.na(Decimal),-"Decimal"]
      if(nrow(icd10SwrongFormat) > 0){
        icd10SwrongFormat<- icd10SwrongFormat[,IcdVersionInFile:="ICD 10"]
        wrongFormatMsg_S<- rbind(wrongFormatMsg_S,icd10SwrongFormat)
        wrongFormat_S <- rbind(wrongFormat_S,icd10SNA[is.na(Decimal),-"Decimal"])
      }
    }else if(nrow(icd10SwrongFormat) > 0){
      wrongFormatMsg_S<- icd10SwrongFormat[,IcdVersionInFile:="ICD 10"]
      wrongFormat_S <- icd10SNA[is.na(Decimal),-"Decimal"]
    }else{
      wrongFormatMsg_S<-NA
      wrongFormat_S <- NA
    }

    if(nrow(icd9SWrongVer) > 0){
      wrongVersionMsg_S <-icd9SWrongVer[,IcdVersionInFile:="ICD 9"]
      wrongVersion_S <- icd9SNA[!is.na(Decimal),-"Decimal"]
      if(nrow(icd10SWrongVer) > 0){
        icd10SWrongVer <- icd10SWrongVer[,IcdVersionInFile:="ICD 10"]
        wrongVersionMsg_S <- rbind(wrongVersionMsg_S,icd10SWrongVer)
        wrongVersion_S <- rbind(wrongVersion_S,icd10SNA[!is.na(Decimal),-"Decimal"])
      }
    }else if(nrow(icd10SWrongVer) > 0){
      wrongVersionMsg_S <- icd10SWrongVer[,IcdVersionInFile:="ICD 10"]
      wrongVersion_S <- icd10SNA[!is.na(Decimal),-"Decimal"]
    }else{
      wrongVersionMsg_S <-NA
      wrongVersion_S <- NA
    }
  }else{
    wrongFormatMsg_S<-NA
    wrongFormat_S <- NA
    wrongVersionMsg_S <-NA
    wrongVersion_S <- NA
  }

  if(!is.null(nrow(wrongFormat_D))){
    allWrongFormat <- wrongFormat_D
    allWrongFormatMsg <- wrongFormatMsg_D[,WrongType:="Wrong format"][order(count,decreasing = T),]
    if(!is.null(nrow(wrongFormat_S))){
      allWrongFormat <- rbind(allWrongFormat,wrongFormat_S)
      wrongFormatMsg_S <- wrongFormatMsg_S[,WrongType:="Wrong format"]
      allWrongFormatMsg<- rbind(allWrongFormatMsg,wrongFormatMsg_S)[order(count,decreasing = T),]
    }
  }else if(!is.null(nrow(wrongFormat_S))){
    allWrongFormat <- wrongFormat_S
    allWrongFormatMsg<- wrongFormatMsg_S[,WrongType:="Wrong format"][order(count,decreasing = T),]
  }else{
    allWrongFormat <- NA
    allWrongFormatMsg <- NA
  }
  if(!is.null(nrow(wrongVersion_D))){
    allWrongVersion <- wrongVersion_D
    allWrongVersionMsg <- wrongVersionMsg_D[,WrongType:="Wrong version"][order(count,decreasing = T),]
    if(!is.null(nrow(wrongVersion_S))){
      allWrongVersion <- rbind(allWrongVersion, wrongVersion_S)
      wrongVersionMsg_S <- wrongVersionMsg_S[,WrongType:="Wrong version"]
      allWrongVersionMsg<- rbind(allWrongVersionMsg, wrongVersionMsg_S)[order(count,decreasing = T),]
    }
  }else if(!is.null(nrow(wrongVersion_S))){
    allWrongVersion <- wrongVersion_S
    allWrongVersionMsg<- wrongVersionMsg_S[,WrongType:="Wrong version"][order(count,decreasing = T),]
  }else{
    allWrongVersion <- NA
    allWrongVersionMsg <- NA
  }
  if(!is.null(nrow(allWrongFormat))){
    allWrongICD <- allWrongFormat
    allWrongICDMsg <- allWrongFormatMsg[order(count,decreasing = T),]
    if(!is.null(nrow(allWrongVersion))){
      allWrongICD <- rbind(allWrongFormat,allWrongVersion)
      allWrongICDMsg <- rbind(allWrongFormatMsg,allWrongVersionMsg)[order(count,decreasing = T),]
    }
  }else if(!is.null(nrow(allWrongVersion))){
    allWrongICD <- allWrongVersion
    allWrongICDMsg <- allWrongVersionMsg[order(count,decreasing = T),]
  }else{
    allWrongICD <- NA
    allWrongICDMsg <- NA
  }
  if(!anyNA(wrongFormatMsg_S) | !anyNA(wrongFormatMsg_D)){
    ICD9wrongFormatMsg <- allWrongICDMsg[grepl("format",allWrongICDMsg$WrongType) & grepl("9",allWrongICDMsg$IcdVersionInFile),]
    ICD9wrongFormatMsg <- ICD9wrongFormatMsg[,Suggestion :=paste0(ICD9wrongFormatMsg[,ICD],"9")]

    ICD9wrongFormatSuggested <- rbind(merge(ICD9wrongFormatMsg[grepl("[.]",ICD9wrongFormatMsg$Suggestion),],
                                            ICD9DxwithTwoFormat,by.x = "Suggestion",by.y = "Decimal",nomatch = T)[,-"Short"],
                                      merge(ICD9wrongFormatMsg[!grepl("[.]",ICD9wrongFormatMsg$Suggestion),],
                                            ICD9DxwithTwoFormat,by.x = "Suggestion",by.y = "Short",nomatch = T)[,-"Decimal"])

    noSuggestedWrongFormat <- allWrongICDMsg[!ICD9wrongFormatSuggested,on = c("ICD","IcdVersionInFile")][,Suggestion :=""]

    allWrongICDMsg <- rbind(noSuggestedWrongFormat,ICD9wrongFormatSuggested)[order(count,decreasing = TRUE)]
  }
  DtoS <- rbind(icd9D[!is.na(Short),-"ICD"], icd10D[!is.na(Short),-"ICD"])
  setnames(DtoS,"Short","ICD")
  StoS <- rbind(icd9S[!is.na(Decimal),-"Decimal"],icd10S[!is.na(Decimal),-"Decimal"])
  allShortFormat <- rbind(StoS, DtoS)

  if(nrow(allShortFormat) < nrow(DxDataFile)){
    if(!is.null(nrow(allWrongFormat))){
      message(paste0("Wrong ICD format: total ",nrow(allWrongFormatMsg)," ICD codes (the number of occurrences is in brackets)"))
      message(head(allWrongFormatMsg[,list(wrongFormat= paste0(ICD," (",count,")","")),],10))
      message(("\t"))
    }
    if(!is.null(nrow(allWrongVersion))){
      message(paste0("Wrong ICD version: total ",nrow(allWrongVersionMsg)," ICD codes (the number of occurrences is in brackets)"))
      message(head(allWrongVersionMsg[,list(wrongFormat= paste0(ICD," (",count,")","")),], 10))
      message(("\t"))
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"Wrong format" means the ICD has wrong format', call. = F)
    warning('"Wrong ICD version" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = F)

    combine_with_error <- rbind(allWrongICD, allShortFormat)[order(Number),"ICD"]
    return(list(ICD = combine_with_error,
                Error = allWrongICDMsg))
  }else{
    return(list(ICD = allShortFormat[order(Number),"ICD"]))
  }
}
