#Mollinetti, 2019

#This script was based on the article From Garcia, Molina, Lozano and Herrera
#"A study on the use of non-parametric tests for analyzing the evolutionary algorithms’ behaviour: a case study
#on the CEC’2005 Special Session on Real Parameter Optimization"

#The results must be the overall mean of the error for the execution 
#of all tests, for all functions

#We assume that the data obtained by the metaheuristics are non-normal and heteroscedastic
#therefore, the need for normality tests such as Shapiro-Wilk or Kolmogorov-Smirnov and for homoscedascity of data like
#the Levine or Durbin-Wilson are not necessary

#Friedman test is first called to check differnce between the means

#Then, Holm and Hochberg procedures from the Bonferroni correction are called in favor of the Wilcoxon paired t-test for nonparametric data

#INPUT: path of the file or csv (multiple paths)

#OUTPUT: list containing:
        #- Friedman test results
        #- Holm test results
        #- Hochberg test results

paired <- function(ctrlpathname, ...){
  
  # install required packages if needed
  packages_needed <- c("dunn.test")
  for (package_name in packages_needed) {      
    if (!(package_name %in% rownames(installed.packages()))){
      install.packages(package_name)
    }
  }
  #call the necessary package
  #dunn.test is responsible for the dunn-bonferroni correction procedure
  library(dunn.test)
  
  
  
  #two strings of the two files that hopefully are in the same folder to be compared
  #Usually, there are no Headers, but it's up to you
  #THE FIRST FILE IS THE CONTROL ALGORITHM, ANY OTHER, WILL BE MEANS OF COMPARISON AGAINST THE CONTROL 
  # FOR THE HOLM AND HOCHBERG PROCEDURE
  
  ctrlpath <- as.character(ctrlpathname)
  others <- list(...)
  
  #ctrlpath<- "ABC"
  #others <- list("Other","Another")
  
  tControl<- read.table(file = ctrlpath, 
                   header = FALSE, 
                   sep = ",")
  
  tControl <- as.numeric(unlist(tControl))
  
  tOthers <- list()
  for (i in 1:length(others))
  {
    temp<- read.table(file = unlist(others[i]), 
                    header = FALSE, 
                    sep = ",")
    tOthers[[i]]<- as.numeric(unlist(temp))
  }
  

  #check the differences of the mean to see if any they have any difference against the control and each other
  #This is done by the Friedman and Iman-Davenport tests
  #null hypothesis is that the data is the same (translating: if p < alpha, data is different)
  c <- matrix(c(tControl,unlist(tOthers, recursive=FALSE)),nrow=length(tControl), dimnames = list(1:length(tControl), c(ctrlpath,unlist(others, recursive=FALSE))))
  fr <-friedman.test(c)
  
  
  l <- apply(c[,1:(length(tOthers)+1)],2,as.list)
  l <- lapply(l,as.numeric)
  l2 <- lapply(l, unlist)
  
  #now do a post-hoc pairwise test to check for statistical difference between the pairs
  #first we'll be doing a Bonferroni-Dunn method with Holm's correction procedure with the control and the rest
  
  bholm <- dunn.test(x=l2, method = "holm")
  
  #then the same Bonferroni-Dunn method but with Hochberg correction with the same control
  #dunn.test(x=list(tControl, tControl,unlist(tOthers, recursive=FALSE)), method = "hochberg")
  
  bhoch<- dunn.test(x=l2, method = "hochberg")
  
 return(list(fr,bholm,bhoch))
}
  
  
  


