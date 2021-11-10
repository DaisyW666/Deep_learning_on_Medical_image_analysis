
rm(list = ls())
gc()
library("rJava")
library("xlsxjars")
library("readxl")
library("ggplot2")
library("ggthemes")

worksheet<-"The open access raw_data_manually_extracted.xlsx in this repository"
Data = read_excel(worksheet,1)

NewReviewData<-Data

NewReviewData <- NewReviewData[!is.na(as.numeric(NewReviewData$sample_size_normalize)),]

NewReviewDataHisto<-data.frame(NewReviewData$sample_size_normalize ,NewReviewData$sample_type)

NewReviewDataHisto$NewReviewData.sample_size_normalize <- as.numeric(as.character(NewReviewDataHisto$NewReviewData.sample_size_normalize))

p<-ggplot(NewReviewDataHisto, aes(NewReviewData.sample_size_normalize,fill=NewReviewData.sample_type))

p+geom_histogram(bins=50,color="#e9ecef", alpha=0.9)+ labs(x = "Sample Size of Deep Learning Studies in the field of Medical Image Analysis", y = "Frequency") +

  theme(legend.position = "bottom") + scale_fill_discrete(name = "Study Design") +
theme(axis.title.x = element_text(size = 15, family = "myFont", color = "Black", face = "bold", vjust = 0.5, hjust = 0.5))+
  theme(axis.title.y = element_text(size = 15, family = "myFont", color = "Black", face = "bold", vjust = 0.5, hjust = 0.5))+
  theme(plot.title = element_text(size = 15, family = "myFont", color = "Black", face = "bold", vjust = 0.5, hjust = 0.5))

