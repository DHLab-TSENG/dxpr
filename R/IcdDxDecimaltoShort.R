if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "Number",
  "Short",
  "Decimal",
  "ICD9DxwithTwoFormat",
  "ICD10DxwithTwoFormat"))
#' Convert ICD Codes From Decimal To Short Forms
#'
#' Convert codes between short and decimal forms
#'
#' @import data.table
#' @param icdList ICD codes
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Quality-Initiatives-Patient-Assessment-Instruments/HospitalQualityInits/Downloads/HospitalAppendix_F.pdf}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-CM.html}
#'
IcdDxDecimalToShort<-function(icdList){
  icdDf <- data.table(ICD = icdList, Number = 1:length(icdList),stringsAsFactors = FALSE)
  icd_Decimal <- icdDf[grep("[.]",icdDf$ICD),]
  icd_Short <- icdDf[!icd_Decimal, on = .(Number)]

  DtoS <- rbind(merge(icd_Decimal, ICD9DxwithTwoFormat, by.x = "ICD",by.y = "Decimal", nomatch = 0),
                merge(icd_Decimal, ICD10DxwithTwoFormat, by.x = "ICD",by.y = "Decimal", nomatch = 0))
  DtoS <- DtoS[,c(2,3)]
  setnames(DtoS,"Short","ICD")
  StoS <- unique(rbind(merge(icd_Short, ICD9DxwithTwoFormat, by.x = "ICD",by.y = "Short", nomatch = 0),
                       merge(icd_Short, ICD10DxwithTwoFormat, by.x = "ICD",by.y = "Short", nomatch = 0)))

  allShortFormat <- rbind(StoS[,Number, ICD], DtoS[,Number, ICD])

  DtoS_wrongFormat <- icd_Decimal[!DtoS,on = .(Number)]
  StoS_wrongFormat <- icd_Short[!StoS,on = .(Number)]

  wrongFormat <- rbind(DtoS_wrongFormat, StoS_wrongFormat)
  wrongFormat <- unique(wrongFormat[order(Number)])
  combine_with_error <- rbind(wrongFormat, allShortFormat)
  combine_with_error <- unique(combine_with_error[order(Number)])

  return(list(Short = combine_with_error$ICD,
              Error = wrongFormat))
}
