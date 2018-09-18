#' Convert ICD Codes From Short To Decimal Forms
#'
#' Convert codes between short and decimal forms
#'
#' @import dplyr
#'
#' @param icdList ICD codes
#'
#'
convertIcdShortToDecimal<-function(icdList){
  icdDf <- data.frame(ICD = icdList, Number = 1:length(icdList),stringsAsFactors = FALSE)
  icd_Short <- data.frame(Short = icdDf$ICD[!grepl("[.]",icdDf$ICD)], Number = icdDf$Number[!grepl("[.]",icdDf$ICD)],stringsAsFactors = FALSE)
  icd_Decimal <- data.frame(Decimal = icdDf$ICD[grepl("[.]",icdDf$ICD)], Number = icdDf$Number[grepl("[.]",icdDf$ICD)],stringsAsFactors = FALSE)

  icd9 <- left_join(icd_Short, icd9withTwoFormat, by = "Short")
  icd10 <- left_join(icd_Short, icd10withTwoFormat, by = "Short")
  Combine <- full_join(icd9[!is.na(icd9$Decimal),], icd10[!is.na(icd10$Decimal),], by = c("Decimal", "Number", "Short"))

  combine_with_Decimal <- full_join(icd_Decimal, Combine, by = c("Number", "Decimal")) %>% arrange(Number)

  error <- anti_join(data.frame(Decimal = icdDf$ICD, Number = icdDf$Number, stringsAsFactors = FALSE),
                     select(combine_with_Decimal, Decimal, Number),
                     c("Number"))
  if(nrow(error) >=1){
    combine_with_Decimal_and_error <- full_join(error, combine_with_Decimal, c("Number","Decimal")) %>% arrange(Number)
    combine_with_Decimal_and_error <- combine_with_Decimal_and_error$Short
    return(combine_with_Decimal_and_error)
  }else{
    combine_with_Decimal <- combine_with_Decimal$Decimal
    return(combine_with_Decimal)
  }
}
