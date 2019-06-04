#'
#' Plot of diagnostic category data
#'
#' @import data.table
#' @import ggplot2
#' @importFrom stats chisq.test
#' @param groupedDataWide groupedData file from functions of code classification (four strategies)
#' @param TopN Default is Top "10"
#' @param limitPrevalance the minimum  of prevalance Default is "0.01", in other words,the limit at same diagnostic category must have 1 percent patient in total patient.
#' @param pvalue p value of chisq.test
#' @export
#' @examples
#' head(sampleDxFile)
#' groupedDataWide <- groupedDataLongToWide(sampleDxFile, ID, ICD, Date,
#'                                          icd10usingDate = "2015-10-01",
#'                                          groupDataType = elix)
#' head(groupedDataWide)
#' plot_groupedData(groupedDataWide = groupedDataWide,
#'                  TopN = 10,
#'                  limitPrevalance = 0.01,
#'                  pvalue = 0.001)
#'
plot_groupedData <- function(groupedDataWide, TopN = 10, limitPrevalance = 0.01, pvalue = 0.05){
  pvalue <- c()
  ggtitle <- "Diagnostic category"
  if(names(groupedDataWide)[ncol(groupedDataWide)] == "selectedCase"){
    groupedDataWide <- cbind(groupedDataWide[, -c(1, ncol(groupedDataWide))]*1,
                             group = groupedDataWide[, "selectedCase"])
    groupedDataLong <- melt(groupedDataWide, id.vars = "group",variable.name = "category", value.name = "value")
    groupedDataLong <- as.data.table(groupedDataLong)[,list(count = sum(value)), by = list(group, category)]

    caseNum <- sum(!grepl("non|[*]",groupedDataWide$group))
    controlNum <- sum(grepl("non",groupedDataWide$group))
    caseDataLong <- groupedDataLong[!grepl("non|[*]",groupedDataLong$group),][,catePerc := round((count/caseNum)*100,2)]
    controlDataLong <- groupedDataLong[grepl("non",groupedDataLong$group),][,catePerc := round((count/controlNum)*100,2)]

    for(cat in 1:nrow(caseDataLong)){
      if(caseDataLong$catePerc[cat] >= limitPrevalance | controlDataLong$catePerc[cat] >= limitPrevalance){
        chisq <- rbind(c(caseDataLong$count[cat], caseNum - caseDataLong$count[cat]),
                       c(controlDataLong$count[cat], controlNum - controlDataLong$count[cat]))
        pvalue[[length(pvalue)+1]] <- chisq.test(chisq, simulate.p.value = T)$p.value < pvalue
      }else{
        pvalue[[length(pvalue)+1]] <- FALSE
      }
    }
    groupedDataLong <- groupedDataLong[,list(sum = sum(count)),by = category][order(sum,decreasing = T)]
    groupedDataLong <- groupedDataLong[1:TopN,][order(sum)][,-"sum"]
    dignosticCate <- merge(groupedDataLong, rbind(caseDataLong[pvalue,],controlDataLong[pvalue,]),all.x = T)
    dignosticCate <- dignosticCate[,c("group","category","catePerc") :=
                                     list(factor(group, levels = unique(dignosticCate$group)),
                                          factor(category, levels = groupedDataLong$category),
                                          paste0(catePerc,"%")),]

    g <- ggplot(dignosticCate, aes(fill =  group, y = count, x = category, group = group)) +
      geom_text(aes(label = paste0(catePerc,"%")), vjust = -0.5, hjust = -0.5, size = 2.5, position = position_dodge(width = 1)) +
      geom_bar(position="dodge", stat="identity")
  }else{
    groupedDataWide[,-1] <- groupedDataWide[,-1]*1
    groupedDataWide$group <- "noGroup"
    groupedDataLong <- melt(groupedDataWide[,-1], id.vars = "group",variable.name = "category", value.name = "value")
    groupedDataLong <- as.data.table(groupedDataLong)[,list(count = sum(value)), by = list(group, category)][order(count)][,catePerc := round(count/nrow(groupedDataWide)*100,2)][,-"group"][catePerc >= limitPrevalance,]
    groupedDataLong <- groupedDataLong[][,c("category","Number","catePerc") :=
                                         list(factor(category, levels = category),
                                              nrow(groupedDataLong):1,
                                              paste0(catePerc,"%")),][Number <= TopN,]

    g <- ggplot(groupedDataLong, aes(y = count, x = category)) +
      geom_text(aes(label = catePerc), vjust = -.5, hjust = -0.5, size = 2.5, position = position_dodge(width = 1))  +
      geom_bar(position="dodge", stat="identity") +
      guides(fill = FALSE, color = FALSE)
    dignosticCate <- groupedDataLong[,-"Number"]
  }
  ggtitle <- paste0(ggtitle,": Top ", TopN)
  dignosticCate_graph <- g +
    coord_flip() +
    xlab("Diagnostic category") + ylab("Diagnostic category, n") + ggtitle(ggtitle) +
    theme_bw() +
    theme(axis.text.y = element_text(size = 10,face = "bold"),
          axis.text.x = element_text(size = 10,face = "bold"))

  return(list(graph = dignosticCate_graph,
              sigCate = dignosticCate[order(count,decreasing = T)]))
}

