#'
#' Plot of diagnostic category data
#'
#' @import data.table
#' @import ggplot2
#' @importFrom stats chisq.test
#' @param groupedDataWide groupedData file from functions of code classification (four strategies)
#' @param Ranking Default is Top "10"
#' @param limitPrevalance the minimum  of prevalance Default is "0.01", in other words,the limit at same diagnostic category must have 1 percent patient in total patient.
#' @export
#' @examples
#' head(sampleDxFile)
#' groupedDataWide <- groupedDataLongToWide(sampleDxFile, ID, ICD, Date,
#'                                          icd10usingDate = "2015-10-01",
#'                                          groupDataType = elix)
#' head(groupedDataWide)
#' plot_groupedData(groupedDataWide = groupedDataWide,
#'                  Ranking = 10,
#'                  limitPrevalance = 0.01)
#'
plot_groupedData <- function(groupedDataWide, Ranking = 10, limitPrevalance = 0.01){
  pvalue <- c()
  ggtitle <- "diagnostic category in GroupedData"
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
        pvalue[[length(pvalue)+1]] <- chisq.test(chisq,simulate.p.value = T)$p.value < 0.001
      }else{
        pvalue[[length(pvalue)+1]] <- FALSE
      }
    }
    groupedDataLong <- groupedDataLong[,list(sum = sum(count)),by = category][order(sum,decreasing = T)]
    groupedDataLong <- groupedDataLong[1:Ranking,][order(sum)][,-"sum"]
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
                                              paste0(catePerc,"%")),][Number <= Ranking,]

    g <- ggplot(groupedDataLong, aes(y = count, x = category)) +
      geom_text(aes(label = catePerc), vjust = -.5, hjust = -0.5, size = 2.5, position = position_dodge(width = 1))  +
      geom_bar(position="dodge", stat="identity", aes(fill = Number)) +
      guides(fill = FALSE, color = FALSE)
    dignosticCate <- groupedDataLong[,-"Number"]
  }
  ggtitle <- paste0(ggtitle,": Top ",Ranking)
  dignosticCate_graph <- g +
    coord_flip() +
    xlab("diagnostic category") + ylab("count of diagnostic category") + ggtitle(ggtitle) +
    theme_bw() +
    theme(axis.text.y = element_text(size = 10,face = "bold"),
          axis.text.x = element_text(size = 10,face = "bold"))

  return(list(graph = dignosticCate_graph,
              sigCate = dignosticCate[order(count,decreasing = T)]))
}

#### poisson.test ----
# if(names(groupedDataWide)[length(groupedDataWide)] == "selectedCase"){
#   groupedDataWide <- cbind(groupedDataWide[,-length(groupedDataWide)]*1,Group = groupedDataWide$selectedCase)
#   case <- groupedDataWide[!grepl("non|[*]",groupedDataWide$Group),]
#   control<- groupedDataWide[grepl("non",groupedDataWide$Group),]
#   caseNum <- nrow(case)
#   controlNum <-  nrow(control)
#
#   for(cat in 2:(length(groupedDataWide)-1)){
#     if(poisson.test(c(sum(case[, cat]), sum(control[, cat])), c(caseNum, controlNum))$p.value > 0.5){
#       pvalue[[length(pvalue)+1]] <- chisq.test(groupedDataWide[,-length(case)])
#     }
#   }
#   pvalue[length(pvalue)+1] <- TRUE
# }else{
#
# }

