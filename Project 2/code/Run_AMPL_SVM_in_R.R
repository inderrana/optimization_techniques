##clean the environment 
rm(list = ls())
gc()
cat("\014")

##################load libs##################
library(dplyr)
library(tidyverse)
library(heatmaply)
library(ggplot2)
##################set working dir##################
work_dir <- "C:/Users/irana/OneDrive - IESEG/Documents/Courses/Semester_2/opt_tech/group_project"
ampl_dir <-"C:/Users/irana/OneDrive - IESEG/Documents/Courses/Semester_2/opt_tech/ampl.mswin64"

setwd(work_dir)

##################read data##################
iris <- read_csv("data/iris.csv", show_col_types = FALSE)

##################normalize data##################
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

##################apply Min-Max normalization to first four columns in iris dataset##################
iris_norm <- as.data.frame(lapply(iris[1:4], min_max_norm))

##################Write the data to csv so that we can feed it to AMPL later##################
write.csv(iris_norm,'data/iris_norm.csv')

##################plot data##################

#names(iris)

ggplot(iris,aes(sepal_length,sepal_width,col=species...7))+geom_point()+geom_abline(intercept = -0.4,slope=0.65)
dev.off()

ggplot(data = iris, aes(sepal_length,sepal_width,col=species...7)) +
  geom_line()+
  theme(text = element_text(size=6), legend.position = "right") +
  xlab(expression(xtoplot[1])) + ylab(expression(xtoplot[2]))
dev.off()

##################RUN AMPL##################

setwd(ampl_dir)
system('ampl -include group_project/svm.run')


#load X vars
X <- read_table("group_project/X.txt", 
                skip = 1)

#load y vars
Y <- read_table("group_project/Y.txt", skip = 1)

#load weights
W <- read_table("group_project/W.txt")

#load gamma
G <- read.table("group_project/G.txt", quote="\"")

#assign indivisual weights to vars
W1 = W$`[*]`[1]
W2 = W$`[*]`[2]
W3 = W$`[*]`[3]
W4 = W$`[*]`[4]

#assign gamma var
Gval =  G$V3

##clean the environment 
#rm(list = ls())
#gc()
#cat("\014")

