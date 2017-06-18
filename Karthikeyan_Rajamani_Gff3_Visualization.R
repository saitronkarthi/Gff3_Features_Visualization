#Take-home exam 2

#Instructions
#1. To complete this exam you will need the following packages
library(data.table,lib.loc="C:\\Users\\Admin\\Documents\\R\\win-library\\3.3")
library(dplyr,lib.loc="C:\\Users\\Admin\\Documents\\R\\win-library\\3.3")
library(ggplot2,lib.loc="C:\\Users\\Admin\\Documents\\R\\win-library\\3.3",)
library(RColorBrewer,lib.loc="C:\\Users\\Admin\\Documents\\R\\win-library\\3.3")  #optional: you can use any colors you want
library(scales,lib.loc="C:\\Users\\Admin\\Documents\\R\\win-library\\3.3") 


#2. Here is a link to the dataset for this exam. Assume I have this file
#in current directory
#https://www.dropbox.com/s/zfvftt3zrz5lnti/C.elegans_new.gff3?dl=0
#Downloaded to the current directory
#3. Save this file as Exam2_YourFirstName_YourLastName.R and submit
#to blackboard by midnight on May 12th.


################## EXAM QUESTIONS ########################

#Q1. Read in C.elegans_new.gff3 with `fread` function
# from {data.table} package. Save data as `C.elegans` R object
C.elegans<-fread(".\\C.elegans_new.gff3")

#Q2. Create a horizontal bar graph that displays average length
#of every genomic feature except for "chromosome" (features are
#specified in column 2)
#Use base R for all your data manipulations and graphics.
#Start with `C.elegans` object you created in Q1.
#Save your plot as "LengthByFeature.pdf" file.
#Your plot should look similar to "LengthByFeature_AW.pdf"
#https://www.dropbox.com/s/u2bt89lemexnnqw/LengthByFeature_AW.pdf?dl=0


###NOTE: In order to fit feature names on your graph, you will
#need to increase left plot margin. You can do it by adding
#`par(mai=c(1.02,1.2,0.82,0.42))` function to your code
#after you open pdf graphics device.

################ Data manipulation with base R ###############
#Provide your code here
C.elegans$flength<-c(C.elegans$Stop-C.elegans$Start+1) # add a new column for the flength
v1<-data.frame(C.elegans)
FeatureAvg<-aggregate(v1$flength,v1[2],mean,na.rm=TRUE) # Average by feature
LBF<-FeatureAvg[FeatureAvg$Feature!="chromosome",] # eliminate the chromosome
LBFsorted<-LBF[order(-LBF$x),TRUE] # sort by ascending order

################### Plotting with base graphics ###############
#Provide your code here
pdf("LengthByFeature.pdf") # open the plot file
par(mai=c(1.02,1.2,0.82,0.42)) # set margin
#rCol <-brewer.pal(8,"Set1") # use color brewer 
# plot the bargraph with options
barplot(LBFsorted$x,names.arg = LBFsorted$Feature,horiz = TRUE, 
        col=rainbow(21),las=2,cex.names=0.6,xlim=c(0,4000),border ="white")
title(main="C.elegans: Average Length of Genomic Features") 
dev.off() # close & save the file

#Q3. Create a plot that displays individual AND mean gene
#lengths for each chromosome.
#Exclude 3 longest genes from your dataset.
#Use dplyr functions with pipes(%>%) for your data
#manipulations and ggplot2 for graphics.
#Start with C.elegans object you created in Q1.
#Save your plot as "LengthByChr.pdf" file.
#Your plot should look similar to "LengthByChr_AW.pdf"
#https://www.dropbox.com/s/yllpid0coy9zi11/LengthByChr_AW.pdf?dl=0

############ Data manipulation with dplyr ################# 
#Provide your code here
###########
Genedatasorted<-filter(C.elegans, Feature=="gene")%>% # Get gene feature from C.elenans
  select(Chr,flength)%>% #select the Chr & lenght columns
  arrange(desc(flength))%>% # arrange in desc order
  slice(-c(1:3)) #eliminate the first 3 longest genes
meanGenedatasorted<-Genedatasorted%>%
  group_by(Chr)%>%
  summarise(flength=mean(flength)) # get mean dataset grouped by chromosome
################### Plotting with ggplot2 graphics ###############
#Provide your code here
############
pdf("LengthByChr.pdf") # open file
#plot with dataset & options
p<-ggplot(data=Genedatasorted, aes(Chr,flength))+geom_point(aes(color=Chr),
                                                            size=5, alpha=0.2,position=position_jitter(width=0.1))
p1<-p+labs(x="Chromosome", y="Gene Length(bp)") # labels for x& Y axis
p2<-p1+ylim(0,60000) # Set the scale
p3<-p2+geom_point(data=meanGenedatasorted, aes(col="black"),size=4)# plot the means
p3 # draw the plot
dev.off() # close & save the file