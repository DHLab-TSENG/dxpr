#' get ICD-9-PCS Codes Decimal Forms
#'
#' Convert codes between short and decimal forms
#' Format ICD-9-PCS Principal and Other Diagnosis Codes can be found at url link.
#'
#' return `ICD9PrwithTwoFormat` for ICD function of conversion
#' @param icd9 ICD-9-PCS codes from prICD9 file
#' @source 2014-ICD-9-PCS
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#'
ICD9PrGenerateDecimalFormat <- function(icd9){
  icd9 <- data.frame(Short = icd9,stringsAsFactors = F)

  icd9_3 <- nchar(icd9$Short) == 3
  icd9$Decimal[icd9_3] <- paste(substr(icd9$Short[icd9_3], start = 1 , stop =  2), ".",
                                substr(icd9$Short[icd9_3], start= 3, stop = 3), sep = "")
  icd9_4 <- nchar(icd9$Short) == 4
  icd9$Decimal[icd9_4] <- paste(substr(icd9$Short[icd9_4], start = 1 , stop =  2), ".",
                                substr(icd9$Short[icd9_4], start= 3, stop = 4), sep = "")
  icd9
}

