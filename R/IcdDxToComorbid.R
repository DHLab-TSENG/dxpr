if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "icd9_ahrq",
  "icd10_ahrq",
  "icd9_charlson",
  "icd10_charlson",
  "icd9_elix",
  "icd10_elix",
  "Comorbidity"))
#' Grouping comorbid method comorbidities (AHRQ, Charlson and Elixhauser Comorbidity) infers whether to use ICD-9 or ICD-10 codes
#'
#' Get comorbidities using the comorbidity methods based on ICD code in clinical diagnostic data.
#'
#' return comorbidity meseaures based on ICD diagnosis codes
#'
#' @import data.table
#' @param DxDataFile A file of clinical diagnostic data with at least 3 columns: "MemberID","ICD", "Date"
#' @param idColName A column for MemberID of DxDataFile
#' @param icdColName A column for ICD of DxDataFile
#' @param dateColName A column for Date of DxDataFile
#' @param icd10usingDate ICD 10 using date
#' @param comorbidMethod  Three comorbidity method: AHRQ, Charlson and Elixhauser Comorbidity, type `ahrq`,`charlson`, or`elix`
#' @export
#' @source AHRQ

#' @source Charlson

#' @source Elixhauser
#' @source ICD-9-CM Elixhauser (2012-2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp#references}
#' @source ICD-10-CM Elixhauser (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp}
#' @examples
#'
#' IcdDxToComorbid(sampleDxFile, ID, ICD, Date, "2015-10-01", charlson)
#' IcdDxToComorbid(sampleDxFile, ID, ICD, Date, "2015-10-01", elix)
#' IcdDxToComorbid(sampleDxFile, ID, ICD, Date, "2015-10-01", ahrq)
#'
IcdDxToComorbid <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, comorbidMethod){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])
  DxDataFile[,Number:=1:nrow(DxDataFile)]
  DxDataFile$Short <- IcdDxDecimalToShort(DxDataFile,ICD,Date,icd10usingDate)$ICD

  comorbidMethod <- tolower(deparse(substitute(comorbidMethod)))
  if (grepl("ahrq", comorbidMethod)){
    comorbidMap9 <- `icd9_ahrq`
    comorbidMap10 <- `icd10_ahrq`
  }else if(grepl("charlson", comorbidMethod)){
    comorbidMap9 <- `icd9_charlson`
    comorbidMap10 <- `icd10_charlson`
  }else if(grepl("elix", comorbidMethod)){
    comorbidMap9 <- `icd9_elix`
    comorbidMap10 <- `icd10_elix`
  }else{
    stop("'please enter AHRQ, Charlson or Elix for 'comorbidMethod'", call. = FALSE)
  }

  IcdToComorbid <- rbind(merge(DxDataFile[Date <icd10usingDate],comorbidMap9[,list(ICD,Comorbidity)],by.x ="Short",by.y = "ICD",all.x = T),
                         merge(DxDataFile[Date >=icd10usingDate],comorbidMap10[,list(ICD,Comorbidity)],by.x ="Short",by.y = "ICD",all.x = T))

  IcdToComorbidLong <- IcdToComorbid[order(Number)&!is.na(Comorbidity),
                                     list(firstCaseDate = min(Date),
                                          endCaseDate = max(Date),
                                          count = .N),
                                     by = list(ID,Comorbidity)][,period := (endCaseDate - firstCaseDate),]

  return(list(groupedDf = IcdToComorbid,
              groupedData_Long = IcdToComorbidLong))
}

