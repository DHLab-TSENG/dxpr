#' Convert ICD-9-PCS Codes From Short To Decimal Forms
#'
#' Return codes with Decimal forms
#'
#' @import dplyr
#'
#' @param icdList only ICD-9-PCS codes have decimal and short forms
#'
IcdPrShortToDecimal<-function(icdList){
  icdDf <- data.frame(ICD = icdList, Number = 1:length(icdList),stringsAsFactors = FALSE)
  icd_Short <- data.frame(Short = icdDf$ICD[!grepl("[.]",icdDf$ICD)], Number = icdDf$Number[!grepl("[.]",icdDf$ICD)],stringsAsFactors = FALSE)
  icd_Decimal <- data.frame(Decimal = icdDf$ICD[grepl("[.]",icdDf$ICD)], Number = icdDf$Number[grepl("[.]",icdDf$ICD)],stringsAsFactors = FALSE)

  icd9_D <- left_join(icd_Decimal, ICD9PrwithTwoFormat, by = "Decimal")
  icd9_S <- left_join(icd_Short, ICD9PrwithTwoFormat, by = "Short")
  icd10_S <- semi_join(icd9_S[is.na(icd9_S$Decimal),], select(ccsPrICD10,ICD), by = c("Short" = "ICD"))
  icd10_S$Decimal <- icd10_S$Short
  combine_S <- rbind(icd9_S[!is.na(icd9_S$Decimal),],icd10_S)
  combine <- rbind(icd9_D[!is.na(icd9_D$Short),],combine_S) %>% arrange(Number)

  error <- anti_join(data.frame(Decimal = icdDf$ICD, Number = icdDf$Number, stringsAsFactors = FALSE),
                     select(combine, Decimal, Number),
                     c("Number")) %>% mutate(Short = NA)

  combine_with_error <- rbind(error, combine) %>% arrange(Number)
  combine_with_error <- combine_with_error$Decimal

  return(list(Decimal = combine_with_error, Error = error$Decimal))
}



