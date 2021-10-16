

setwd("D:")

install.packages("psych")
library(psych)
install.packages("plyr")
library(plyr)
install.packages("reshape")
library(reshape)
install.packages("sos") #invaluable findFn() in the 'sos' package which is 
                        #an extension for R's inbuilt help
library(sos)


 

#load the data set

dir()
pi <- read_stata("pisa_2012.dta")

View(pi)
names(pi)


##################################################################################
#### (1.) Missing Values
#How many missing values do the plausible values for maths, reading and science have?
#What else can
#you say about missing values in this dataset?


### (1) Missing values of the plausible value variables


#question 1: missing values
#Maths 
sum(is.na(pi$PV1MATH)) #0
sum(is.na(pi$PV2MATH)) #0
sum(is.na(pi$PV3MATH)) #0
sum(is.na(pi$PV4MATH)) #0
sum(is.na(pi$PV5MATH)) #0

#READING 
sum(is.na(pi$PV1READ))
sum(is.na(pi$PV2READ))
sum(is.na(pi$PV3READ))
sum(is.na(pi$PV4READ))
sum(is.na(pi$PV5READ))
# all 0

#Science 
sum(is.na(pi$PV1SCIE))
sum(is.na(pi$PV2SCIE))
sum(is.na(pi$PV3SCIE))
sum(is.na(pi$PV4SCIE))
sum(is.na(pi$PV5SCIE))
# all 0


### (2) Missing values of other variables

#give a list of all variables with their corresponding number of missing values

colSums(is.na(pi))

#We see (by manually looking at it):
#missing values for age, grade, ST06Q01,  SCMAT, ANXMAT, FAILMAT, INSTMOT, INTMAT,
#MATBEH, MATHEFF, MATINTFC, PERSEV, OPENPS, SUBNORM, FAMCON


## Check by calculating (non-manually) the number of variables
## which have missing values:

#create a data set containing only the number of missing values of each variable
check <- data.frame(colSums(is.na(pi)))
View(check)
# count all variables which have missing values
sum(test1$test>0)
#15 -> same as manual count above, ok!


## Calculate the percentage of missing values

#total number of observations:
nrow(pi) #23087

mi <- colSums(is.na(pi)/nrow(pi))
#in percent:
mi_p <- mi*100
mi_p

#age: 0.126

#grade: 0.819

#ST06Q01: 6.493

#SCMAT: 34.937

#ANXMAT: 34.950

#FAILMAT: 35.011

#INSTMOT: 34.682

#INTMAT: 34.643

#MATBEH: 34.959

#MATHEFF: 34.734

#MATINTFC: 38.584

#PERSEV: 35.141

#OPENPS: 35.197

#SUBNORM: 34.678

#FAMCON: 27.920



##################################################################################
#### (2.) Estimation Procedure of Plausible Values and Country-Level Average Scores


####################################################################################
#### Understand the estimation procedure


## Copy the provided function:

#2 lines of code that we have to run before:

pi <- pi[ , order(names(pi))]
names(pi)[331] <- "W_FSTUWT"

## take care to only run these lines ONCE (or change the name of the data set)
##because otherwise running these lines will keep changing your data set

#>>> Function pisa >>>

pisa_config <- list("variables","parameters")
pisa_config$variables$weightFinal<-"W_FSTUWT"
pisa_config$variables$weightBRR <-"W_FSTR"
pisa_config$parameters$BRRreps <- as.numeric(80) # number of replication weights

#>>> Functions >>>

#>>> Mean estimation for plausible values

fun.pv <- function (pvnames, data, folder = getwd()) {
  pv.input <- function(pvnames, data) {
    
    R.mean <- sapply(pvnames, function(k) sapply(1:pisa_config$parameters$BRRreps, 
                                                 function(i) weighted.mean(data[[k]], 
                                                                           data[[paste0(pisa_config$variables$weightBRR,i)]], na.rm = TRUE)))
    R.sd <- sapply(pvnames, function(x) sapply(1:pisa_config$parameters$BRRreps, 
                                               function(i) (sum(data[[paste0(pisa_config$variables$weightBRR,i)]] * (data[[x]] - R.mean[i, x])^2, na.rm = TRUE)
                                                            /sum(data[[paste0(pisa_config$variables$weightBRR,i)]], na.rm = TRUE))^(1/2)))
    PV.mean <- sapply(pvnames, function(x) weighted.mean(data[[x]], 
                                                         data[[pisa_config$variables$weightFinal]], na.rm = TRUE))
    PV.sd <- sapply(pvnames, function(x) (sum(data[[pisa_config$variables$weightFinal]] * 
                                                (data[[x]] - PV.mean[x])^2, na.rm = TRUE)/sum(data[[pisa_config$variables$weightFinal]], 
                                                                                              na.rm = TRUE))^(1/2))
    MEAN.m <- mean(PV.mean)
    SD.m <- mean(PV.sd)
    cc = 1/20
    
    var.mean.w <- mean(sapply(seq_along(pvnames), function(i) cc *sum((R.mean[, i] - PV.mean[i])^2)))
    
    var.mean.b <- (1/(length(pvnames) - 1)) * sum(sapply(seq_along(pvnames), function(i) (PV.mean[i] - MEAN.m)^2)) 
    
    mean.se <- (var.mean.w + (1 + 1/length(pvnames)) * var.mean.b)^(1/2)
    
    LB <- MEAN.m - 1.96*mean.se
    UB <- MEAN.m + 1.96*mean.se
    
    result <- data.frame(Freq = length(data[[pisa_config$variables$weightFinal]]), 
                         Mean = mean(MEAN.m), s.e. = mean.se, LB = LB, UB = UB)
    
    return(round(result, 2))
  }
  
  output <- pv.input(pvnames = pvnames, data = data)
  
  class(output) <- c("intsvy.mean", "data.frame")
  return(output)
}





####################################################################################
#### Compute the Country-Average Scores (for Finland, France, Norway and Vietnam)

## now just apply the function created above


pv.math <- c("PV1MATH", "PV2MATH", "PV3MATH", "PV4MATH", "PV5MATH")
pv.read <- c("PV1READ", "PV2READ", "PV3READ", "PV4READ", "PV5READ")
pv.scien <- c("PV1SCIE", "PV2SCIE", "PV3SCIE", "PV4SCIE", "PV5SCIE")

### Math

Fi_m <- fun.pv(pv.math, pi[pi$CNT=="Finland", ] , folder= getwd())
Fr_m <- fun.pv(pv.math, pi[pi$CNT=="France", ] , folder= getwd())
No_m <- fun.pv(pv.math, pi[pi$CNT=="Norway", ] , folder= getwd())
Vi_m <- fun.pv(pv.math, pi[pi$CNT=="Viet Nam", ] , folder= getwd())

#We can bind the scores together
math <- as.data.frame(rbind(Fi_m, Fr_m, No_m, Vi_m))
View(math)

## save the table
install.packages("xtable")
library(xtable)
#use the xtable command to create LaTeX code:
xtable(math)
#copy the LaTex Code into the table generate ("file" -> "from LaTex Code")


### Reading

Fi_r<-fun.pv(pv.read, pi[pi$CNT=="Finland", ] , folder= getwd())
Fr_r<-fun.pv(pv.read, pi[pi$CNT=="France", ] , folder= getwd())
No_r<-fun.pv(pv.read, pi[pi$CNT=="Norway", ] , folder= getwd())
Vi_r<-fun.pv(pv.read, pi[pi$CNT=="Viet Nam", ] , folder= getwd())

#bind the scores together
reading <- rbind(Fi_r, Fr_r, No_r, Vi_r)
View(reading)

##save the table
xtable(reading)


### Sciences

Fi_s <- fun.pv(pv.scien, pi[pi$CNT=="Finland", ] , folder= getwd())
Fr_s <- fun.pv(pv.scien, pi[pi$CNT=="France", ] , folder= getwd())
No_s <- fun.pv(pv.scien, pi[pi$CNT=="Norway", ] , folder= getwd())
Vi_s <- fun.pv(pv.scien, pi[pi$CNT=="Viet Nam", ] , folder= getwd())

science <- rbind(Fi_s, Fr_s, No_s, Vi_s)
View(science)

##save the table
xtable(science)


######################################################################################
#### (3.) Average Scores in Reading, Mathematics,
####      and Science for Girls and Boys Separately

# Just apply the function again, add the gender condition

#### Males

### Math

Fi_mM <- fun.pv(pv.math, pi[pi$CNT=="Finland" & pi$GENDER==1, ] , folder= getwd())
Fr_mM <- fun.pv(pv.math, pi[pi$CNT=="France" & pi$GENDER==1, ] , folder= getwd())
No_mM <- fun.pv(pv.math, pi[pi$CNT=="Norway" & pi$GENDER==1, ] , folder= getwd())
Vi_mM <- fun.pv(pv.math, pi[pi$CNT=="Viet Nam" & pi$GENDER==1, ] , folder= getwd())

#bind them together
math_m <- rbind(Fi_mM, Fr_mM, No_mM, Vi_mM)
View(math_m)


### Reading

Fi_rM <- fun.pv(pv.read, pi[pi$CNT=="Finland" & pi$GENDER==1, ] , folder= getwd())
Fr_rM <- fun.pv(pv.read, pi[pi$CNT=="France" & pi$GENDER==1, ] , folder= getwd())
No_rM <- fun.pv(pv.read, pi[pi$CNT=="Norway" & pi$GENDER==1, ] , folder= getwd())
Vi_rM <- fun.pv(pv.read, pi[pi$CNT=="Viet Nam" & pi$GENDER==1, ] , folder= getwd())

#Bind them together
reading_m <- rbind(Fi_rM, Fr_rM, No_rM, Vi_rM)
View(reading_m)

### Science

Fi_sM <- fun.pv(pv.scien, pi[pi$CNT=="Finland" & pi$GENDER==1, ] , folder= getwd())
Fr_sM <- fun.pv(pv.scien, pi[pi$CNT=="France" & pi$GENDER==1, ] , folder= getwd())
No_sM <- fun.pv(pv.scien, pi[pi$CNT=="Norway" & pi$GENDER==1, ] , folder= getwd())
Vi_sM <- fun.pv(pv.scien, pi[pi$CNT=="Viet Nam" & pi$GENDER==1, ] , folder= getwd())

#bind them together
science_m <- rbind(Fi_sM, Fr_sM, No_sM, Vi_sM)
View(science_m)


#### Female 

### Math

Fi_mF <- fun.pv(pv.math, pi[pi$CNT=="Finland" & pi$GENDER==0, ] , folder= getwd())
Fr_mF <- fun.pv(pv.math, pi[pi$CNT=="France" & pi$GENDER==0, ] , folder= getwd())
No_mF <- fun.pv(pv.math, pi[pi$CNT=="Norway" & pi$GENDER==0, ] , folder= getwd())
Vi_mF <- fun.pv(pv.math, pi[pi$CNT=="Viet Nam" & pi$GENDER==0, ] , folder= getwd())

#bind them
math_f <- rbind(Fi_mF, Fr_mF, No_mF, Vi_mF)
View(math_f)

### Reading

Fi_rF <- fun.pv(pv.read, pi[pi$CNT=="Finland" & pi$GENDER==0, ] , folder= getwd())
Fr_rF <- fun.pv(pv.read, pi[pi$CNT=="France" & pi$GENDER==0, ] , folder= getwd())
No_rF <- fun.pv(pv.read, pi[pi$CNT=="Norway" & pi$GENDER==0, ] , folder= getwd())
Vi_rF <- fun.pv(pv.read, pi[pi$CNT=="Viet Nam" & pi$GENDER==0, ] , folder= getwd())

#bind them
reading_f <- rbind(Fi_rF, Fr_rF, No_rF, Vi_rF)
View(reading_f)


### Science

Fi_sF <- fun.pv(pv.scien, pi[pi$CNT=="Finland" & pi$GENDER==0, ] , folder= getwd())
Fr_sF <- fun.pv(pv.scien, pi[pi$CNT=="France" & pi$GENDER==0, ] , folder= getwd())
No_sF <- fun.pv(pv.scien, pi[pi$CNT=="Norway" & pi$GENDER==0, ] , folder= getwd())
Vi_sF <- fun.pv(pv.scien, pi[pi$CNT=="Viet Nam" & pi$GENDER==0, ] , folder= getwd())

#bind them together
science_f <- rbind(Fi_sF, Fr_sF, No_sF, Vi_sF)
View(science_f)


### create a meaningful table

# a good way to present things would be a table for each country containing scores
# in each category of boys and girls in two seperate lines

##Finland
Finland <- rbind(Fi_mM, Fi_mF, Fi_rM, Fi_rF, Fi_sM, Fi_sF)
View(Finland)

xtable(Finland)

##France

France <- rbind(Fr_mM, Fr_mF, Fr_rM, Fr_rF, Fr_sM, Fr_sF)
View(France)

xtable(France)


##Norway

Norway <- rbind(No_mM, No_mF, No_rM, No_rF, No_sM, No_sF)
View(Norway)

xtable(Norway)


##Vietnam

Vietnam <- rbind(Vi_mM, Vi_mF, Vi_rM, Vi_rF, Vi_sM, Vi_sF)
View(Vietnam)

xtable(Vietnam)




######################################################################################
#### (4.) Comparison of Differences between Girls and Boys across Countries
####      Find a meaningful representation

# -> (1) Compute average scores of boys and girls in general, NOT per subject
# -> (2) Put them into a histogram

### (1) Compute average scores of boys and girls in general

# we already have the mean of plausible values for each category for each country
# -> for each country, respectively, just take the mean across catagories, i.e.
# take the mean of the means


##Finland

#Boys:

Fi_M <- (1/3)*(Fi_mM + Fi_rM + Fi_sM)
#NOTE: the standard errors cannot be interpreted here
#516.28

#Girls:

Fi_F <- (1/3)*(Fi_mF + Fi_rF + Fi_sF)
#543.2633

##France

#Boys:

Fr_M <- (1/3)*(Fr_mM + Fr_rM + Fr_sM)
#493.3467

#Girls:

Fr_F <- (1/3)*(Fr_mF + Fr_rF + Fr_sF)
#505.9267

##Norway

#Boys:

No_M <- (1/3)*(No_mM + No_rM + No_sM)
#488.1567

#Girls:

No_F <- (1/3)*(No_mF + No_rF + No_sF)
#504.136

##Vietnam

#Boys:

Vi_M <- (1/3)*(Vi_mM + Vi_rM + Vi_sM)
#512.45

#Girls:

Vi_F <- (1/3)*(Vi_mF + Vi_rF + Vi_sF)
#519.07

test <-  cbind(Vi_Mn, Vi_Fn)

### (2) Put them into a histogram

#do it in Excel!





######################################################################################
#### (5.) Self-Assessment in Mathematics: Comparison of Boys and Girls
####      along these Variables and across Countries

#look at the INTMAT function:
summary(pi$INTMAT)


## copy and run the fun function:



#>>> Mean estimation for standard variables

fun <- function(variable, data) {
  
  meanrp <- sapply(1:pisa_config$parameters$BRRreps, function(i) weighted.mean(as.numeric(data[[variable]]), 
                                                                               data[[paste(pisa_config$variables$weightBRR, i, sep = "")]], 
                                                                               na.rm = TRUE))
  sdrp <- sapply(1:pisa_config$parameters$BRRreps, function(i) (sum(data[[paste0(pisa_config$variables$weightBRR,i)]] 
                                                                    * (data[[variable]] - meanrp[i])^2, na.rm = TRUE)
                                                                /sum(data[[paste0(pisa_config$variables$weightBRR,i)]], na.rm = TRUE))^(1/2))
  meantot <- weighted.mean(as.numeric(data[[variable]]), 
                           data[[pisa_config$variables$weightFinal]], na.rm = TRUE)
  
  meanse <- (0.05 * sum((meanrp - meantot)^2))^(1/2)
  
  LB <- meantot - 1.96*meanse
  UB <- meantot + 1.96*meanse
  
  result <- data.frame(Freq = sum(!is.na(data[[variable]])), 
                       Mean = meantot, s.e. = meanse, LB = LB, UB = UB)
  return(round(result, 2))
  
}


### apply the fun function

test <- fun("INTMAT", pi)
test
#ok that is how it works


##Finland

#Girls
pi_f_g <- subset(pi, pi$GENDER==0 & pi$CCODE=="FIN")
f_g <- fun("INTMAT", pi_f_g)

#Boys
pi_f_b <- subset(pi, pi$GENDER==1 & pi$CCODE=="FIN")
f_b <- fun("INTMAT", pi_f_b)

##France

#Girls
pi_fr_g <- subset(pi, pi$GENDER==0 & pi$CCODE=="FRA")
fr_g <- fun("INTMAT", pi_fr_g)

#Boys
pi_fr_b <- subset(pi, pi$GENDER==1 & pi$CCODE=="FRA")
fr_b <- fun("INTMAT", pi_fr_b)

##Norway

#Girls
pi_n_g <- subset(pi, pi$GENDER==0 & pi$CCODE=="NOR")
n_g <- fun("INTMAT", pi_n_g)

#Boys
pi_n_b <- subset(pi, pi$GENDER==1 & pi$CCODE=="NOR")
n_b <- fun("INTMAT", pi_n_b)

##Vietnam

#Girls
pi_v_g <- subset(pi, pi$GENDER==0 & pi$CCODE=="VNM")
v_g <- fun("INTMAT", pi_v_g)

#Boys
pi_v_b <- subset(pi, pi$GENDER==1 & pi$CCODE=="VNM")
v_b <- fun("INTMAT", pi_v_b)



final <- rbind(f_g, f_b, fr_g, fr_b, n_g, n_b, v_g, v_b)

xtable(final)


