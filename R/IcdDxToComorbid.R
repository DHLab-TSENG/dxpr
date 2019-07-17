#'
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
#' @param comorbidMethod  Three comorbidity method: AHRQ, Charlson and Elixhauser Comorbidity. Change
#' @param isDescription   Categories/Description of comorbidities for ICD-9 or ICD-10. By default it is set to \code{True}.
#' it to any of the other possible variables (\code{'ahrq'},\code{'charlson'}, \code{'elix'}).
#' @export
#' @source AHRQ
#' @source ICD-9-CM Elixhauser (2012-2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp#references}
#' @source ICD-10-CM Elixhauser (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp}
#' @source Charlson
#' @source ICD-9-CM Charlson (2006)
#' @source \url{http://mchp-appserv.cpe.umanitoba.ca/Upload/SAS/ICD9_E_Charlson.sas.txt}
#' @source ICD-10-CM Charlson (2006)
#' @source \url{http://mchp-appserv.cpe.umanitoba.ca/Upload/SAS/ICD10_Charlson.sas.txt}
#' @source Elixhauser
#' @source ICD-9-CM Elixhauser (2012-2015)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp#references}
#' @source ICD-10-CM Elixhauser (2019)
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp}
#' @examples
#' head(sampleDxFile)
#' IcdDxToComorbid(sampleDxFile, ID, ICD, Date, "2015-10-01", charlson)
#'
IcdDxToComorbid <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, comorbidMethod, isDescription = TRUE){
  DxDataFile <- as.data.table(DxDataFile)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")
  DxDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(DxDataFile))]
  Conversion <- IcdDxDecimalToShort(DxDataFile,ICD,Date,icd10usingDate)
  DxDataFile[,Short := Conversion$ICD]

  comorbidMethod <- tolower(deparse(substitute(comorbidMethod)))
  if(comorbidMethod == "ahrq"){
    comorbidMap9 <- `icd9_ahrq`
    comorbidMap10 <- `icd10_ahrq`
  }else if(comorbidMethod == "charlson"){
    comorbidMap9 <- `icd9_charlson`
    comorbidMap10 <- `icd10_charlson`
  }else if(comorbidMethod == "elix"){
    comorbidMap9 <- `icd9_elix`
    comorbidMap10 <- `icd10_elix`
  }else{
    stop("'please enter AHRQ, Charlson or Elix for 'comorbidMethod'", call. = FALSE)
  }

  if (isDescription){
    com_col <- "Description"
  }else{
    com_col <- "Comorbidity"
  }

  allComorbid <- rbind(merge(DxDataFile[Date <icd10usingDate], comorbidMap9[,c("ICD",com_col),with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                       merge(DxDataFile[Date >=icd10usingDate], comorbidMap10[,c("ICD",com_col),with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  allComorbid <- allComorbid[order(Number),-"Number"]

  if(nrow(allComorbid[is.na(eval(parse(text = paste(com_col))))]) < nrow(allComorbid)){
    summarisedComorbid <- allComorbid[!is.na(eval(parse(text = paste(com_col)))),
                                      list(firstCaseDate = min(Date),
                                           endCaseDate = max(Date),
                                           count = .N),
                                      by = c("ID",com_col)][,period := (endCaseDate - firstCaseDate),][order(ID),]
    return(list(groupedDT = allComorbid,
                summarised_groupedDT = summarisedComorbid,
                Error = Conversion$Error))
  }else{
    return(list(groupedDT = allComorbid,
                Error = Conversion$Error))
  }
}
