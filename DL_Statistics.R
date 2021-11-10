
##The following code is used to analyze the application of various deep learning networks in various cancer sites,
##and to construct a heat map to represent their relationship with each radiology tasks.

rm(list = ls())
gc()
library("readxl")
library("networkD3")
library("yaml")
library("xlsx")
library("stringr")
library("ggplot2")
library("pheatmap")

rad_sub<-c("skin","thyroid","abdominal", "multiple sites","musculoskeletal","head and neck","breast","eye","thoracic","cardiac","genitourinary","miscellaneous","neuroradiology")

#The correlated items for cluster and heap map. This file is derived from the open access excel A in this repository.
workbook<-"B_Items derived from A for HeatMap.xlsx"
Sta = read_excel(workbook,1)
CorrleaData <-Sta

#47 deep learning networks
workbook1<-"DLnetworks.xlsx"
Sta1 = read_excel(workbook1,1)
DLIndex<-Sta1
LenofDLIndex<-dim(DLIndex)
LenofDL<-LenofDLIndex[1]

#Eight radiology tasks
workbook1<-"Tasks.xlsx" 
Sta1 = read_excel(workbook1,1)
TaskIndex<-Sta1
LenofTaskIndex<-dim(TaskIndex)
LenofTI<-LenofTaskIndex[1]

ClusterData <- as.data.frame(array(0,c(length(rad_sub),LenofDL))) 
rownames(ClusterData) = as.character(as.matrix(rad_sub))
colnames(ClusterData) = as.character(as.matrix(DLIndex[,1]))

clusterColnames<-as.data.frame(array(0,c(LenofDL,LenofTI))) 
rownames(clusterColnames) = as.character(as.matrix(DLIndex[,1]))
table1<-CorrleaData[6]
rad_sub1<-table1[!duplicated(table1, fromLast=TRUE), ] ##Remove duplicates
colnames(clusterColnames) = as.character(as.matrix(rad_sub1))

CorrleaData1<-CorrleaData
LenofCorrlea1<-dim(CorrleaData1)
LenofCorrlea1<-LenofCorrlea1[1]

for( i in 1:LenofCorrlea1)
{
  metadata<-CorrleaData1[i,]
  s<-metadata$Site
  t<-metadata$DLName
  p<-metadata$Value
  if(!is.null(ClusterData[s,t]))
  {
    ClusterData[s,t]<-ClusterData[s,t]+1
    clusterColnames[t,p]<-clusterColnames[t,p]+1
  }

}

###clustering and heat map building

library(pheatmap)

ClusterDataSelect<-ClusterData

data<-data.matrix(ClusterDataSelect) 
LenofCluster<-dim(ClusterDataSelect)
LenofCluster<-LenofCluster[2]

a<-data.frame(colSums(ClusterDataSelect))##check zero

col_anno = data.frame(clusterColnames)

colnames(col_anno) = c("Classification","Detection","Diagnosis","Identification","Image acquisition","Monitoring","Pre-processing","Segmentation")

HEATMAP<-pheatmap(data,cutree_col = 9,annotation=col_anno,cellheight=15,treeheight_row=0,show_colnames=TRUE,
                  clustering_method = "ward",annotation_legend = FALSE,
                  color = colorRampPalette(c( "white", "navy","firebrick3"))(102),
                  fontsize=13,fontsize_row=15,cellwidth = 15,
                  filename="Your path to save the heat map")
