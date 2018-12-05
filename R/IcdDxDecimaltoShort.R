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
#' @import dplyr
#' @param icdList ICD codes
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Quality-Initiatives-Patient-Assessment-Instruments/HospitalQualityInits/Downloads/HospitalAppendix_F.pdf}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-CM.html}
#'
IcdDxDecimalToShort<-function(icdList){
  icdDf <- data.frame(ICD = icdList, Number = 1:length(icdList),stringsAsFactors = FALSE)
  icd_Decimal <- icdDf[grepl("[.]",icdDf$ICD),]
  icd_Short <- anti_join(icdDf,icd_Decimal,"Number")

  DtoS <- rbind(inner_join(icd_Decimal, ICD9DxwithTwoFormat, by = c("ICD" = "Decimal")),
                inner_join(icd_Decimal, ICD10DxwithTwoFormat, by = c("ICD" = "Decimal")))

  StoS <- rbind(semi_join(icd_Short, ICD9DxwithTwoFormat, by = c("ICD" = "Short")),
                semi_join(icd_Short, ICD10DxwithTwoFormat, by = c("ICD" = "Short"))) %>% unique

  allShortFormat <- rbind(StoS, select(DtoS, Number, ICD = Short))

  DtoS_wrongFormat <- anti_join(icd_Decimal, DtoS, by = c("ICD","Number"))
  StoS_wrongFormat <- anti_join(icd_Short, StoS, by = c("ICD","Number"))

  wrongFormat <- rbind(DtoS_wrongFormat, StoS_wrongFormat) %>% arrange(Number)
  combine_with_error <- rbind(wrongFormat, allShortFormat) %>% arrange(Number)

  return(list(Short = combine_with_error$ICD,
              Error = wrongFormat))
}
