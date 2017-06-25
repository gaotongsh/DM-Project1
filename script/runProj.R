################
# DM Proj1     #
# Gao Tong     #
# Chen Yazheng #
################

print("##################################################")
print("# Now Begins Proj1 of DM Course                  #")
print("# Please Install Packages Listed in README First #")
print("##################################################")

source("statAndPlot.R")

print("1.1 Get Raw Data Frame...")
data1.1 <- getDataFrame()
print("...Finished! Result in 'data1.1'")

print("1.2 Preprocess with tm...")
data1.2 <- getPreprocessedData(data1.1)
print("...Finished! Result in 'data1.2'")

print("1.3 Bag of words...")
data1.3 <- getBagOfWord(data1.2)
print("...Finished! Result in 'data1.3'")

print("1.4 Word Cloud & 1.5 Word Length...")
plotWordPlots(data1.3)
print("...Finished!")

print("1.6 Category Histogram...")
cat.table <- plotCategory(data1.3)
print("...Finished! Result in 'cat.table'")

print("1.7 Month Histogram...")
month.table <- plotMonth(data1.3)
print("...Finished! Result in 'month.table'")

print("2.1 Similarity Matrix...")
sim <- similarity.bog(data1.3)
print("...Finished! Result in 'sim'")

print("2.2 In-category Similarity...")
incat.sim <- incat.similarity(data1.3, sim)
print("...Finished! Result in 'incat.sim'")

print("2.3 Inter-category Similarity...")
inter.cat.sim <- inter.cat.similarity(data1.3, sim)
print(paste("...Finished! Average similarity between 'Travel' and 'Theater' is",
            inter.cat.sim))
