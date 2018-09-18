if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "Number",
  "Short",
  "Decimal",
  "icd9withTwoFormat",
  "icd10withTwoFormat"))
#' Convert ICD Codes From Decimal To Short Forms
#'
#' Convert codes between short and decimal forms
#'
#' @import dplyr
#'
#' @param icdList ICD codes
#'
convertIcdDecimaltoShort<-function(icdList){
  icdDf <- data.frame(ICD = icdList, Number = 1:length(icdList),stringsAsFactors = FALSE)
  icd_Short <- data.frame(Short = icdDf$ICD[!grepl("[.]",icdDf$ICD)], Number = icdDf$Number[!grepl("[.]",icdDf$ICD)],stringsAsFactors = FALSE)
  icd_Decomal <- data.frame(Decimal = icdDf$ICD[grepl("[.]",icdDf$ICD)], Number = icdDf$Number[grepl("[.]",icdDf$ICD)],stringsAsFactors = FALSE)

  icd9 <- left_join(icd_Decomal, icd9withTwoFormat, by = "Decimal")
  icd10 <- left_join(icd_Decomal, icd10withTwoFormat, by = "Decimal")
  Combine <- full_join(icd9[!is.na(icd9$Short),], icd10[!is.na(icd10$Short),], by = c("Decimal", "Number", "Short"))

  combine_with_Short <- full_join(icd_Short, Combine,by = c("Number", "Short")) %>% arrange(Number)

  error <- anti_join(data.frame(Short = icdDf$ICD, Number = icdDf$Number, stringsAsFactors = FALSE),
                     select(combine_with_Short,Short,Number),
                     c("Number"))
  if(nrow(error) >=1){
    combine_with_Short_and_error <- full_join(error, combine_with_Short, c("Number","Short")) %>% arrange(Number)
    combine_with_Short_and_error <- combine_with_Short_and_error$Short
    return(combine_with_Short_and_error)
  }else{
    combine_with_Short <- combine_with_Short$Short
    return(combine_with_Short)
  }
}
