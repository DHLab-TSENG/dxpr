#' @rdname dxCustom
#' @export
#'
icdDxToCustomGrep <- function(dxDataFile, idColName, icdColName, dateColName, customGroupingTable){
  GrepedIcd <- as.data.table(dxDataFile)
  customGroupingTable <- as.data.table(customGroupingTable)
  dataCol  <-c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  GrepedIcd <- GrepedIcd[,dataCol,with = FALSE]
  names(GrepedIcd) <- c("ID", "ICD", "Date")
  GrepedIcd[,c("Date", "Number", "GrepedGroup") := list(as.Date(Date), 1:nrow(GrepedIcd), NA)]
  ifelse(is.na(GrepedIcd$Date), stop("NA is detected. Please make sure all values in ICD column are non-null and in the correct date format."),GrepedIcd$Date)
  ifelse(is.na(GrepedIcd$ICD), stop("NA is detected. Please make sure all values in ICD column are non-null."),GrepedIcd$ICD)

  for (rule in 1:nrow(customGroupingTable)){
    GrepedIcd <- GrepedIcd[,GrepedGroup := ifelse(grepl(customGroupingTable[rule,"grepIcd"],GrepedIcd$ICD), customGroupingTable[rule,Group], GrepedIcd$GrepedGroup)]
  }

  if(sum(!is.na(GrepedIcd$GrepedGroup)) > 0){
    summarisedGrepedIcd <- GrepedIcd[nchar(GrepedGroup)>0,
                                     list(firstCaseDate = min(Date),
                                          endCaseDate = max(Date),
                                          count = .N),by = list(ID,GrepedGroup)][,period := (endCaseDate - firstCaseDate),][order(ID),]

    return(list(groupedDT = GrepedIcd[order(Number),-"Number"],
                summarised_groupedDT = summarisedGrepedIcd))
  }else{
    warning("There is no match diagnostic code with the grepTable")
    return(groupedDT = GrepedIcd[order(Number),-"Number"])
  }
}
