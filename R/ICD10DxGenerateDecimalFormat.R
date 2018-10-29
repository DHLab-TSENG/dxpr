#' get ICD-10-CM Codes Decimal Forms
#'
#' Convert codes between short and decimal forms.
#' Format ICD-10-CM Principal and Other Diagnosis Codes can be found at url link.
#'
#' return `ICD10DxwithTwoFormat` for ICD function of conversion
#' @param icd10 ICD-10-CM codes from dxICD10 file
#' @source 2019-ICD-10-CM
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10/2019-ICD-10-CM.html}
#' @source \url{https://www.findacode.com/search/search.php}
#'
ICD10DxGenerateDecimalFormat <- function(icd10){
  icd10 <-data.frame(Short = icd10, stringsAsFactors = FALSE)
  icd10_3 <- nchar(icd10$Short) == 3
  icd10$Decimal[icd10_3] <- ""

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
