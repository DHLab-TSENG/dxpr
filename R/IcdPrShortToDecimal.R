#' Convert ICD-9-PCS Codes From Short To Decimal Forms
#'
#' Return codes with Decimal forms
#'
#' @import data.table
#' @param PrDataFile A file of clinical diagnostic data with at least 2 columns: "ICD", "Date"
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate icd 10 using date
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Quality-Initiatives-Patient-Assessment-Instruments/HospitalQualityInits/Downloads/HospitalAppendix_F.pdf}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-PCS.html}
#' @export
#' @examples
#' head(samplePrFile)
#' IcdPrShortToDecimal(samplePrFile,ICD,Date,"2015/10/01")
#'
IcdPrShortToDecimal<-function(PrDataFile, icdColName, dateColName, icd10usingDate){
  DataCol <- c(deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  PrDataFile <- PrDataFile[,DataCol,with = FALSE]
  names(PrDataFile) <- c("ICD", "Date")
  PrDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(PrDataFile))]

  icd_Decimal <- PrDataFile[grepl("[.]",PrDataFile$ICD),]
  if(nrow(icd_Decimal) > 0){
    icd9D <- merge(icd_Decimal[Date < icd10usingDate], ICD9PrwithTwoFormat, by.x = "ICD", by.y = "Decimal", all.x = T)
    icd10DNA <- merge(icd_Decimal[Date >= icd10usingDate], ICD9PrwithTwoFormat, by.x = "ICD", by.y = "Decimal", all.x = T)

    if(anyNA(icd9D)){icd9DNA <- icd9D[is.na(Short),]}
    if(exists("icd9DNA")){icd9DwrongFormat <- icd9DNA[, list(count = .N), by = ICD]} ###
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

  icd_Short <- PrDataFile[!icd_Decimal, on = "Number"]
  if(nrow(icd_Short) > 0){
    icd9S <- merge(icd_Short[Date < icd10usingDate], ICD9PrwithTwoFormat, by.x = "ICD", by.y = "Short", all.x = T)
    icd10S <- merge(icd_Short[Date >= icd10usingDate], prICD10, by =  "ICD", all.x = T)
    if(anyNA(icd9S)){icd9SNA <- merge(icd9S[is.na(Decimal),-"Decimal"], prICD10,by = "ICD",all.x = T)}
    if(anyNA(icd10S)){icd10SNA <- merge(icd10S[is.na(ICD_DESCRIPTION),-"ICD_DESCRIPTION"], ICD9PrwithTwoFormat,by.x = "ICD", by.y = "Short", all.x = T)}

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

  if(nrow(allDecimalFormat) < nrow(PrDataFile)){
    if(exists("allWrongFormat")){
      message(paste0("Wrong ICD format: total ",nrow(allWrongFormatMsg)," ICD codes (the number of occurrences is in brackets)"))
      allWrongFormatMsg <- allWrongFormatMsg[order(count,decreasing = T),]
      message(head(allWrongFormatMsg[,list(wrongFormat= paste0(ICD," (",count,")","")),],10))
      message(("\t"))
    }
    if(exists("allWrongVersion")){
      message(paste0("Wrong ICD version: total ",nrow(allWrongVersionMsg)," ICD codes (the number of occurrences is in brackets)"))
      allWrongVersionMsg <- allWrongVersionMsg[order(count,decreasing = T),]
      message(head(allWrongVersionMsg[,list(wrongFormat= paste0(ICD," (",count,")","")),], 10))
      message(("\t"))
    }
    warning('The ICD mentioned above matches to "NA" due to the format or other issues.', call. = F)
    warning('"Wrong ICD format" means the ICD has wrong format', call. = F)
    warning('"Wrong ICD version" means the ICD classify to wrong ICD version (cause the "icd10usingDate" or other issues)', call. = F)

    combine_with_error <- rbind(allWrongICD, allDecimalFormat)[order(Number),-"Number"]
    return(list(ICD = combine_with_error,
                Error = allWrongICDMsg[order(count,decreasing = T),]))
  }else{
    return(list(ICD = allDecimalFormat[order(Number),-"Number"]))
  }
}
# samplePrFile <- PrDataFile
# PrDataFile <- rbind(PrDataFile,data.table(ICD = c("79929","45612","12333"),Date = rep(as.Date("2014-10-11"),3),Number = 180:182))
