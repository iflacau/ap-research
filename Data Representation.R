#creating all the visuals baby


#GENERATING GRAPHS
setwd("/Users/ilincaflacau/APResearch/Charts")

#distribution of values
dev.set(dev.next())
pdf(file = 'Distribution Chart.pdf', width = 10, height = 10)
vals <- results[1,4,]
target_indexes <- which(vals <= 10e-12)


#labels
xl <- expression(Log[10] ~ ~ k[D])
title <- expression(Distribution ~ of ~ Dissociation ~ Constant ~ (k[D]) ~ Values) 

hist(log10(vals), breaks = 25, col = "burlywood4", main = title, xlab = xl)
dev.off()

#Normality Test
shapiro.test(log10(vals))

#TABLES
library(gridExtra)
library(grid)

#make the table
filteredMolNames <- c()
for (i in length(target_indexes)) {
  index <- match(matrix_names[target_indexes[i]], idNamesMatch[,1])
  filteredMolNames <- idNamesMatch[index,2]
}
print(filteredMolNames)

filteredTable <- data.frame(idNamesMatch[target_indexes,2], c(results[1,4,target_indexes]))
names(filteredTable) <- c("ID", "kD")
filteredTable <- filteredTable[order(filteredTable$kD),]
print(filteredTable)

#export the table

write.csv(filteredTable, "/Users/ilincaflacau/APResearch/Charts/ResultMoleculesTable.csv", row.names = FALSE)