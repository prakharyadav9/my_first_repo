#Artificial neural networks example

#the concrete dataset contains 1,030 examples of concrete with eight features describing the
#components used in the mixture. These features are thought to be related to the final 
#compressive strength and they include the amount (in kilograms per cubic meter) of cement, 
#slag, ash, water, superplasticizer, coarse aggregate, and fine aggregate used in the product 
#in addition to the aging time (measured in days).

#Importing Excel files using xlsx package
#The xlsx package, a java-based solution, is one of the powerful R packages 
#to read, write and format Excel files
##Using xlsx package
#There are two main functions in xlsx package for reading both xls and xlsx Excel files:
#read.xlsx() and read.xlsx2() [faster on big files compared to read.xlsx function].

#The simplified formats are:
  
 # read.xlsx(file, sheetIndex, header=TRUE)
#read.xlsx2(file, sheetIndex, header=TRUE)

#file: file path
#sheetIndex: the index of the sheet to be read
#header: a logical value. If TRUE, the first row is used as column names.


#Example of usage:
  
  library(xlsx)
#my_data <- read.xlsx(file.choose(), 1)  # read first sheet
  #R xlsx package : A quick start guide to manipulate Excel files in R
#r2excel package: Read, write and format easily Excel files using R software

install.packages("xlsx")



install.packages("readxl")
library(readxl)
concrete <- read_excel("C:\\Users\\prakh\\Desktop\\Concrete_Data.xls")
head(concrete)
summary(concrete)
str(concrete)
dim(concrete) #tells rows * columns
table(concrete$`Concrete compressive strength(MPa, megapascals)`)
# Neural networks work best when the input data are scaled to a narrow range around zero,
#and here, we see values ranging anywhere from zero up to over a thousand. 
concrete_norm <- as.data.frame(lapply(concrete,normalize))
colnames(concrete) <- c("cement","blast","Flyash","water","spr_plasticizer","crse_aggr",
                        "fn_aggr","age","strength")
summary(concrete_norm$strength)

#since the data set is already in random order so we just need to divide it into train n test data
concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]
#To model the relationship between the ingredients used in concrete and the strength ofthe 
#finished product, we will use a multilayer feedforward neural network. The neuralnet package 
install.packages("neuralnet")
library(neuralnet)
concrete_model <- neuralnet(strength~cement+blast+Flyash+water+spr_plasticizer
                            +crse_aggr+fn_aggr+age,data = concrete_train,hidden = 1)
#a neural ntwrk object is returned to make predictions
plot(concrete_model)
# To generate predictions on the test dataset, we can use the compute() as follows:
#The compute() function works a bit differently from the predict() functions we've used so
#far. It returns a list with two components: $neurons, which stores the neurons for each
#layer in the network, and $net.result,which stores the predicted values. We'll want the latter

model_results <- compute(concrete_model,concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength,concrete_test$strength)

#Correlations close to 1 indicate strong linear relationships between two variables. 
#Therefore, the correlation here of about 0.715 indicates a fairly strong relationship.
#This implies that our model is doing a fairly good job, even with only a single hidden node. 
#IMPROVING MODEL PERFORMANCE by increasing no of hidden layers
concrete_model2 <- neuralnet(strength~cement+blast+Flyash+water+spr_plasticizer
                             +crse_aggr+fn_aggr+age,data = concrete_train,hidden = 6)
plot(concrete_model2)
model_results2 <- compute(concrete_model2,concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2,concrete_test$strength)
