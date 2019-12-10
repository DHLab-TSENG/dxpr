#' @rdname dxCustom
#' @export
#'
icdDxToCustom <- function(dxDataFile, idColName, icdColName, dateColName, customGroupingTable){
  customICD <- as.data.table(dxDataFile)
  customGroupingTable <- as.data.table(customGroupingTable)
  dataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  customICD <- customICD[,dataCol,with = FALSE]
  names(customICD) <- c("ID", "ICD", "Date")
  customICD[,c("Date", "Number") := list(as.Date(Date), 1:nrow(customICD))]

  groupedICD <- merge(customICD, customGroupingTable, by = "ICD", all.x = TRUE)[order(Number), -"Number"]

  if(sum(!is.na(groupedICD$Group)) > 0){
    summarisedgroupedICD <- groupedICD[!is.na(Group),
                                       list(firstCaseDate = min(Date),
                                            endCaseDate = max(Date),
                                            count = .N),
                                       by = list(ID, Group)][,period := (endCaseDate - firstCaseDate),][order(ID),]
    return(list(groupedDT = groupedICD,
                summarised_groupedDT = summarisedgroupedICD))
  }else{
    warning("There is no match diagnostic code with the groupingTable")
    return(groupedDT = groupedICD)
  }
}
