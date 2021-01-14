# Author: Colin Tang
#
# function to make a distribution plot from the DepMap expression cell line data
# inputs are file name, gene name for the data, and labels for cell lines
#

library("ggplot2")
library("wesanderson")
library("ggrepel")

DepMapPlot<-function(csvfile, genename, labels = c()) {
  datadf <- read.csv(csvfile)
  datadf$labels <- ""
  colnames(datadf) <- c("ID", "Expression", "Cell_Line","Primary_Disease", "Lineage", "All_Primary_Disease","Label")
  if (length(labels) > 0) {
    cellRows <- match(labels, datadf$Cell_Line)
    datadf$Label[cellRows] <- as.character(datadf$Cell_Line[cellRows])
  }
  g <- ggplot(datadf, aes(x = Expression, y = Primary_Disease, label = Label, color=Primary_Disease, fill=Primary_Disease))
  graph <- g + geom_violin(scale = "area", alpha = 0.5, color = NA) + theme_bw() + geom_jitter(height = 0.1, width = 0.1, alpha = 0.7) + 
    geom_text_repel(color = "red", force = 5, min.segment.length = 0, box.padding = 1) + xlab(paste(genename,"Expression Log2(TPM + 1)")) +
    theme(legend.position = "none") + scale_color_manual(values = wes_palette("Darjeeling1", length(levels(datadf$Primary_Disease)), type = "continuous"))+
    scale_fill_manual(values = wes_palette("Darjeeling1", length(levels(datadf$Primary_Disease)), type = "continuous"))
  print(graph)
  ggsave("Depmap_Expression.pdf", width = 7, height = 5)
}
