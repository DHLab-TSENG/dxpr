#'
#' Plot of diagnostic category data
#'
#' @import data.table
#' @import ggplot2
#' @importFrom stats chisq.test
#' @importFrom stats fisher.test
#' @param groupedDataWide groupedData file from functions of code classification (four stPercentagegies)
#' @param TopN Default is Top "10"
#' @param limitPercentage the minimum  of Percentage Default is "0.01", in other words,the limit at same diagnostic category must have 1 percent patient in total patient.
#' @param pvalue p value of chisq.test
#' @export
#' @examples
#' head(sampleDxFile)
#' groupedDataWide <- groupedDataLongToWide(sampleDxFile, ID, ICD, Date,
#'                                          icd10usingDate = "2015-10-01",
#'                                          groupDataType = elix)
#' plot1 <- plot_groupedData(groupedDataWide = groupedDataWide,
#'                           TopN = 10,
#'                           limitPercentage = 0.01)
#'
#' selectedCaseFile <- selectCases(DxDataFile = sampleDxFile,
#'                                 idColName = ID,
#'                                 icdColName = ICD,
#'                                 dateColName = Date,
#'                                 icd10usingDate = "2015/10/01",
#'                                 groupDataType = ccslvl2,
#'                                 caseCondition = "Diseases of the urinary system",
#'                                 caseCount = 1)
#' groupedDataWide <- groupedDataLongToWide(sampleDxFile, ID, ICD, Date,
#'                                          icd10usingDate = "2015-10-01",
#'                                          groupDataType = elix,
#'                                          selectedCaseFile = selectedCaseFile)
#' plot2 <- plot_groupedData(groupedDataWide = groupedDataWide,
#'                           TopN = 10,
#'                           limitPercentage = 0.01,
#'                           pvalue = 0.05)
#' plot1
#' plot2
#'
plot_groupedData <- function(groupedDataWide, TopN = 10, limitPercentage = 0.01, pvalue = 0.05){
  Test_pvalue <- c()
  plot_title <- "Diagnostic category"
  groupedDataWide <- groupedDataWide[,-1]
  if(names(groupedDataWide)[ncol(groupedDataWide)] == "selectedCase"){
    if(is.numeric(groupedDataWide[[1,1]])){
      groupedDataWide <- cbind(as.data.frame(groupedDataWide[,1:(ncol(groupedDataWide)-1)] >= 1L),
                               group = groupedDataWide[, "selectedCase"])
    }
    groupedDataWide <- cbind(groupedDataWide[, -c(ncol(groupedDataWide))]*1,
                             group = groupedDataWide[, "selectedCase"])

    groupedDataLong <- melt(groupedDataWide, id.vars = "group",variable.name = "category", value.name = "value")
    groupedDataLong <- as.data.table(groupedDataLong)[,list(count = sum(value)), by = list(group, category)]

    caseNum <- sum(!grepl("non|[*]",groupedDataWide$group))
    controlNum <- sum(grepl("non",groupedDataWide$group))
    caseDataLong <- groupedDataLong[!grepl("non|[*]",groupedDataLong$group),][,catePerc := round((count/caseNum)*100,2)]
    controlDataLong <- groupedDataLong[grepl("non",groupedDataLong$group),][,catePerc := round((count/controlNum)*100,2)]

    for(cat in 1:nrow(caseDataLong)){
      if(caseDataLong$catePerc[cat] >= limitPercentage | controlDataLong$catePerc[cat] >= limitPercentage){
        Table <- matrix(c(caseDataLong$count[cat], controlDataLong$count[cat],
                          caseNum - caseDataLong$count[cat], controlNum - controlDataLong$count[cat]), 2, 2)

        if(sum(Table < 5) < 1){
          Test_pvalue[[length(Test_pvalue)+1]] <- chisq.test(Table)$p.value < pvalue
        }else{
          Test_pvalue[[length(Test_pvalue)+1]] <- fisher.test(Table, alternative = "greater")$p.value < pvalue
        }
      }else{
        Test_pvalue[[length(Test_pvalue)+1]] <- FALSE
      }
    }
    if(sum(Test_pvalue) == 0){
      return(message("There is no significant category between case and control"))
    }else{
      groupedDataLong <- groupedDataLong[,list(sum = sum(count)),by = category][order(sum,decreasing = T)]
      groupedDataLong <- groupedDataLong[1:TopN,][order(sum),"category"]
      groupedDataLong[,"category" := factor(category, levels = groupedDataLong$category),]

      dignosticCate <- merge(groupedDataLong, rbind(caseDataLong[Test_pvalue,],controlDataLong[Test_pvalue,]),all.x = T)[!is.na(group),]
      dignosticCate[,c("group","catePerc") := list(factor(group, levels = unique(dignosticCate$group)),
                                                   paste0(catePerc,"%")),]

      g <- ggplot(dignosticCate, aes(fill =  group, y = count, x = category, group = group)) +
        geom_text(aes(label = catePerc), hjust = -.2, size = 3, position = position_dodge(width = 1)) +
        geom_bar(position="dodge", stat="identity")
    }
  }else{
    if(is.numeric(groupedDataWide[[1,1]])){
      groupedDataWide <- as.data.frame(groupedDataWide >= 1L)
    }
    groupedDataWide <- groupedDataWide*1
    groupedDataWide$group <- "noGroup"
    groupedDataLong <- melt(groupedDataWide, id.vars = "group",variable.name = "category", value.name = "value")
    groupedDataLong <- as.data.table(groupedDataLong)[,list(count = sum(value)), by = list(group, category)][order(count)][,catePerc := round(count/nrow(groupedDataWide)*100,2)][,-"group"][catePerc >= limitPercentage,]
    groupedDataLong <- groupedDataLong[][,c("category","Number","catePerc") :=
                                         list(factor(category, levels = category),
                                              nrow(groupedDataLong):1,
                                              paste0(catePerc,"%")),][Number <= TopN,]
    dignosticCate <- groupedDataLong[,-"Number"]

    g <- ggplot(groupedDataLong, aes(y = count, x = category)) +
      geom_bar(position="dodge", stat="identity") +
      geom_text(aes(label = catePerc), hjust = -.2, size = 3, position = position_dodge(width = 1))
  }
  plot_title <- paste0(plot_title,": Top ", TopN)
  dignosticCate_graph <- g + coord_flip() +
    xlab("Diagnostic category") + ylab("Diagnostic category, n") + ggtitle(plot_title) +
    annotate("rect",xmin = 1,xmax = 1, ymin = 1, ymax = 25, fill = "white") +
    theme_bw() +
    theme(axis.text.y = element_text(size = 10,face = "bold"),
          axis.text.x = element_text(size = 10,face = "bold"))

  return(list(graph = dignosticCate_graph,
              sigCate = dignosticCate[order(count,decreasing = T)]))
}

