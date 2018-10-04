#' get ICD-9-CM Codes Decimal Forms
#'
#' Convert codes between short and decimal forms
#' Format ICD-9-CM Principal and Other Diagnosis Codes can be found at url link.
#'
#' @param icd9 ICD-9-CM codes
#' @source \url{https://www.findacode.com/search/search.php}
#' @source \url{https://www.cms.gov/Medicare/Quality-Initiatives-Patient-Assessment-Instruments/HospitalQualityInits/Downloads/HospitalAppendix_F.pdf}
#'
ICD_9_CM_GenerateDecimalFormat <- function(icd9){
  icd9 <- data.frame(Short = icd9,stringsAsFactors = F)
  icdNcharLong <- 4
  icd9V <- data.frame(Short = icd9[grepl("^V", icd9$Short),],stringsAsFactors = F)
  icd9N <- data.frame(Short = icd9[grepl("^[0-9]+", icd9$Short),],stringsAsFactors = F)

  decimalstart <- icdNcharLong - 1
  decimalstop <- icdNcharLong
  V4 <- nchar(icd9V$Short) == icdNcharLong
  icd9V$Decimal[V4] <- paste(substr(icd9V$Short[V4], start = 1 , stop =  decimalstart), ".",
                             substr(icd9V$Short[V4], start= decimalstop, stop = icdNcharLong), sep = "")
  N4 <- nchar(icd9N$Short) == icdNcharLong
  icd9N$Decimal[N4] <- paste(substr(icd9N$Short[N4], start = 1 , stop =  decimalstart), ".",
                             substr(icd9N$Short[N4], start= decimalstop, stop = icdNcharLong), sep = "")
  icdNcharLong <- 5
  decimalstart <- icdNcharLong - 2
  decimalstop<- icdNcharLong - 1
  V5 <- nchar(icd9V$Short) == icdNcharLong
  icd9V$Decimal[V5] <- paste(substr(icd9V$Short[V5], start = 1 , stop =  decimalstart), ".",
                             substr(icd9V$Short[V5], start= decimalstop, stop = icdNcharLong), sep = "")
  N5 <- nchar(icd9N$Short) == icdNcharLong
  icd9N$Decimal[N5] <- paste(substr(icd9N$Short[N5], start = 1 , stop =  decimalstart), ".",
                             substr(icd9N$Short[N5], start= decimalstop, stop = icdNcharLong), sep = "")

  icd9E <- data.frame(Short = icd9[grepl("^E", icd9$Short),],stringsAsFactors = F)
  decimalstart <- icdNcharLong - 1
  decimalstop <- icdNcharLong
  E5 <- nchar(icd9E$Short) == icdNcharLong
  icd9E$Decimal[E5] <- paste(substr(icd9E$Short[E5], start = 1 , stop =  decimalstart), ".",
                             substr(icd9E$Short[E5], start= decimalstop, stop = icdNcharLong), sep = "")
  icd9_VN <- full_join(icd9V, icd9N, by = c("Short", "Decimal"))
  icd9_combine <- full_join(icd9E, icd9_VN, by = c("Short", "Decimal"))
  icd9_combine
}
