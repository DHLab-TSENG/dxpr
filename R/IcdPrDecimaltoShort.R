if(getRversion() >= "2.15.1") utils::globalVariables(c(
"ICD9PrwithTwoFormat",
"prICD10"))
#' Convert ICD-9-PCS Codes From Decimal To Short Forms
#'
#' Return codes with short forms
#'
#' @import dplyr
#' @param icdList only ICD-9-PCS codes have decimal and short forms
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Quality-Initiatives-Patient-Assessment-Instruments/HospitalQualityInits/Downloads/HospitalAppendix_F.pdf}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-PCS.html}
#'
IcdPrDecimalToShort<-function(icdList){
  icdDf <- data.frame(ICD = icdList, Number = 1:length(icdList),stringsAsFactors = FALSE)
  icd_Decimal <- icdDf[grepl("[.]",icdDf$ICD),]
  icd_Short <- anti_join(icdDf,icd_Decimal,"Number")

  DtoS <- inner_join(icd_Decimal, ICD9PrwithTwoFormat, by = c("ICD" = "Decimal"))
  StoS <- rbind(semi_join(icd_Short, ICD9PrwithTwoFormat, by = c("ICD" = "Short")),
                semi_join(icd_Short, select(prICD10,ICD), by = "ICD")) %>% unique

  allShortFormat <- rbind(select(DtoS,Number,ICD=Short), StoS)

  DtoS_wrongFormat <- anti_join(icd_Decimal, DtoS, by = c("ICD","Number"))
  StoS_wrongFormat <- anti_join(icd_Short, StoS, by = c("ICD","Number"))

  wrongFormat <- rbind(DtoS_wrongFormat, StoS_wrongFormat) %>% arrange(Number)
  combine_with_error <- rbind(wrongFormat, allShortFormat) %>% arrange(Number)

  return(list(Short = combine_with_error$ICD, Error = wrongFormat))
}
