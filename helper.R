
#install.packages("xlsx") 
#install.packages("dplyr") 
#install.packages("lubridate") 
#install.packages("tidyr") 
#install.packages("stringr") 

library(xlsx)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# fonctions Data -------------------------------------------------------
read.data <- function() {
  
  data<- read.xlsx2("./train.xlsx", sheetIndex = 1,
                         colClasses = c("numeric", "numeric", 
                                        "numeric","numeric", 
                                        "numeric","numeric",
                                        "numeric","numeric",
                                        "numeric","numeric",
                                        "numeric","numeric",
                                        "numeric","numeric",
                                        "numeric","numeric",
                                        "numeric","numeric",
                                        "numeric","numeric",
                                        "numeric","numeric",
                                        "numeric","numeric",
                                        "numeric","numeric",
                                        "numeric","numeric",
                                        "numeric","numeric",
                                        "numeric")) 
  data
}
  
               
read.test <- function() {  
  
                    test<- read.xlsx2("./test.xlsx", sheetIndex = 1,
                                      colClasses = c("numeric", "numeric", 
                                                     "numeric","numeric", 
                                                     "numeric","numeric",
                                                     "numeric","numeric",
                                                     "numeric","numeric",
                                                     "numeric","numeric",
                                                     "numeric","numeric",
                                                     "numeric","numeric",
                                                     "numeric","numeric",
                                                     "numeric","numeric",
                                                     "numeric","numeric",
                                                     "numeric","numeric",
                                                     "numeric","numeric",
                                                     "numeric","numeric",
                                                     "numeric","numeric",
                                                     "numeric"))
test
                    
}
                        
 
