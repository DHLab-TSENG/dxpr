#' @rdname DxCustom
#' @export
#'
IcdDxToCustom <- function(DxDataFile, idColName, icdColName, dateColName, CustomGroupingTable){
  customICD <- as.data.table(DxDataFile)
  CustomGroupingTable <- as.data.table(CustomGroupingTable)
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  customICD <- customICD[,DataCol,with = FALSE]
  names(customICD) <- c("ID", "ICD", "Date")
  customICD[,c("Date", "Number") := list(as.Date(Date), 1:nrow(customICD))]

  groupedICD <- merge(customICD, CustomGroupingTable, by = "ICD", all.x = TRUE)[order(Number), -"Number"]

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
