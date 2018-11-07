#' Convert ICD Codes From Short To Decimal Forms
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

IcdDxShortToDecimal<-function(icdList){
  icdDf <- data.frame(ICD = icdList, Number = 1:length(icdList),stringsAsFactors = FALSE)
  icd_Decimal <- icdDf[grepl("[.]",icdDf$ICD),]
  icd_Short <- icdDf[!grepl("[.]",icdDf$ICD),]

  DtoD <- semi_join(icd_Decimal, ICD9DxwithTwoFormat, by = c("ICD" = "Decimal"))
  StoD <- inner_join(icd_Short, ICD9DxwithTwoFormat, by = c("ICD" = "Short"))

  DtoD_wrongFormat <- anti_join(icd_Decimal, DtoD, by = c("ICD","Number"))
  StoD_wrongFormat <- anti_join(icd_Short, StoD, by = c("ICD","Number"))

  allDecimalFormat <- rbind(DtoD, select(StoD, Number, ICD = Decimal))

  wrongFormat <- rbind(DtoD_wrongFormat, StoD_wrongFormat) %>% arrange(Number)
  combine_with_error <- rbind(wrongFormat, allDecimalFormat) %>% arrange(Number)

  return(list(Decimal = combine_with_error$ICD,
              Error = wrongFormat))
}
