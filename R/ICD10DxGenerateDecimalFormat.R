#' get ICD-10-CM Codes Decimal Forms
#'
#' Convert codes between short and decimal forms.
#' Format ICD-10-CM Principal and Other Diagnosis Codes can be found at url link.
#'
#' @param icd10 ICD-10-CM codes
#' @source \url{https://www.findacode.com/search/search.php}
#'
ICD10DxGenerateDecimalFormat <- function(icd10){
  icd10 <-data.frame(Short = icd10, stringsAsFactors = FALSE)
  icd10_4 <- nchar(icd10$Short) == 4
  icd10$Decimal[icd10_4] <- paste(substr(icd10$Short[icd10_4], start = 1 , stop =  3), ".",
                                  substr(icd10$Short[icd10_4], start= 4, stop = 4), sep = "")
  icd10_5 <- nchar(icd10$Short) == 5
  icd10$Decimal[icd10_5] <- paste(substr(icd10$Short[icd10_5], start = 1 , stop =  3), ".",
                                  substr(icd10$Short[icd10_5], start= 4, stop = 5), sep = "")
  icd10_6 <- nchar(icd10$Short) == 6
  icd10$Decimal[icd10_6] <- paste(substr(icd10$Short[icd10_6], start = 1 , stop =  3), ".",
                                  substr(icd10$Short[icd10_6], start= 4, stop = 6), sep = "")
  icd10_7 <- nchar(icd10$Short) == 7
  icd10$Decimal[icd10_7] <- paste(substr(icd10$Short[icd10_7], start = 1 , stop =  3), ".",
                                  substr(icd10$Short[icd10_7], start= 4, stop = 7), sep = "")
  icd10
}

