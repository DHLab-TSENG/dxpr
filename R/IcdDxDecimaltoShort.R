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
IcdDxDecimaltoShort<-function(icdList){
  icdDf <- data.frame(ICD = icdList, Number = 1:length(icdList),stringsAsFactors = FALSE)
  icd_Short <- data.frame(Short = icdDf$ICD[!grepl("[.]",icdDf$ICD)], Number = icdDf$Number[!grepl("[.]",icdDf$ICD)],stringsAsFactors = FALSE)
  icd_Decimal <- data.frame(Decimal = icdDf$ICD[grepl("[.]",icdDf$ICD)], Number = icdDf$Number[grepl("[.]",icdDf$ICD)],stringsAsFactors = FALSE)

  icd9_D <- left_join(icd_Decimal, ICD9DxwithTwoFormat, by = "Decimal")
  icd10_D <- left_join(icd_Decimal, ICD10DxwithTwoFormat, by = "Decimal")
  combine_D <- rbind(icd9_D[!is.na(icd9_D$Short),], icd10_D[!is.na(icd10_D$Short),])

  icd9_S <- left_join(icd_Short, ICD9DxwithTwoFormat, by = "Short")
  icd10_S <- left_join(icd_Short, ICD10DxwithTwoFormat, by = "Short")
  combine_S <- rbind(icd9_S[!is.na(icd9_S$Decimal),], icd10_S[!is.na(icd10_S$Decimal),])

  combine <- rbind(combine_D,combine_S) %>% arrange(Number)

  error <- anti_join(data.frame(Short = icdDf$ICD, Number = icdDf$Number, stringsAsFactors = FALSE),
                     select(combine, Short, Number),
                       c("Number")) %>% mutate(Decimal = NA)
  combine_with_error <- rbind(error, combine) %>% arrange(Number)
  combine_with_error <- combine_with_error$Short

  return(list(Short = combine_with_error, Error = error$Short))
}
