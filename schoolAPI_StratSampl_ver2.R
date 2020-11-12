library(survey)
data(api)
head(apipop)
#library(corrplot)
#install.packages("corrplot")


### CHOOSING A VARIABLE OF INTEREST#########
######## lets visualize the survey data and see which varibale might be more interesting #####
par(mfrow=c(1,3))
boxplot(meals~stype, data=apipop, ylab="Percentage of students eligible for subsidized meals", xlab="School type")

boxplot(ell~stype, data=apipop, ylab="‘English Language Learners’ (percent)", xlab="School type")
boxplot(col.grad~stype, data=apipop, ylab="Number of college graduates", xlab="School type")

dev.off()


props= table(apipop$stype)/length(apipop$stype)

########## SRS SAMPLING###################

######### theoretical values#########

# population mean
mean(apipop$meals) 

# sample sd
S_sqrt= (1/ (nrow(apipop)-1))*sum( (apipop$meals -mean(apipop$meals))^2)
S_sqrt
sqrt(S_sqrt)

var_ybar = (1 - (sum(round(props*100))/nrow(apipop)))*(( sd(apipop$meals)^2 )/sum(round(props*100)))
sd_ybar = sqrt(var_ybar)


# sample mean  or estimate mean
#sum(sample(apipop$meals, sum(round(props*100)) ))/sum(round(props*100))


######## simple random sampling from apipop dataset specifically the meals column##############

pop_srs<-function(data, Nsim, prop){
  xbars_pop = NA
  #SE_xbars_pop = NA
  for( i in 1:Nsim){srs=sample(data$meals, sum(round(props*100)))
  xbars_pop[i] <- mean(srs) 
  #SE_xbars_pop[i] <- sd(srs)
  
  }
  
  
  output <- list(xbars_pop,mean(xbars_pop), sd(xbars_pop)) # mean(SE_xbars_pop)
  names(output) <- c("sample xbars_pop","mean_xbars_pop",'se of xbars') # "mean_SE_xbars_pop"
  return(output)
}


srsAPI= pop_srs(apipop, 10000, props)
srsAPI








######## DATA PREPARATION for Stratified############
####### separated data by school type to make it easier during sampling
apipop$N <- NA

apipop$N[apipop$stype=='E'] <- length(apipop$sname[apipop$stype=='E'])
apipop$N[apipop$stype=='M'] <- length(apipop$sname[apipop$stype=='M'])
apipop$N[apipop$stype=='H'] <- length(apipop$sname[apipop$stype=='H'])



##### first lets find the proportion of schools that are Elementary, Middle school and High school.
# This is needed to do a proportional stratified sampling using school type as strata. 

props= table(apipop$stype)/length(apipop$stype); props
props


# lets multiply the above proportions by 100 and round to get an estimate of how many of each school type we should sample 

round(props*100)
#### reordered the apipop dataset to make it easier to sample 
elem_data = apipop[apipop$stype=='E',]
midd_data = apipop[apipop$stype=='M',]
high_data = apipop[apipop$stype=='H',]

# When indexing apipop_reorder from 1:4421 are elementary schools, 4422:5439 are middle schools and 5440:6194 are high schools
apipop_reorder = rbind(elem_data, midd_data,high_data) 


################# Theoretical calculations of ybar and sigma of ybar ####
mus=tapply(apipop$meals,apipop$stype ,mean)
SDs=tapply(apipop$meals,apipop$stype ,sd)

len_Es=length(apipop$sname[apipop$stype=='E'])
len_Ms=length(apipop$sname[apipop$stype=='M'])
len_Hs=length(apipop$sname[apipop$stype=='H'])

ybar_str = (1/nrow(apipop))*( len_Es*mus[[1]]  +  len_Ms*mus[[3]]+ len_Hs*mus[[2]]  )

var_ybar_str = (1/nrow(apipop)^2)* ( ((len_Es^2)*(1- (71/len_Es))*((SDs[[1]]^2)/71))  +
                                       ((len_Ms^2)*(1- (16/len_Ms))*((SDs[[3]]^2)/16)) +
                                       ((len_Hs^2)*(1- (12/len_Hs))*( (SDs[[2]]^2)/12)) )
sd_ybar_str = sqrt(var_ybar_str)





######### STRATIFIED SAMPLING#######################
######### proportional stratified sampling from apipop dataset############
strat_sampl_pop<- function(data, Nsim, prop){
  xbar_EMH <- NA
  SE_xbar_EMH <- NA
  for(i in 1:Nsim){sampl_elem= data[1:4421,][sample(nrow(data[1:4421,]), round(props[[1]]*100)),] 
  sampl_midd=  data[4422:5439,][sample(nrow(data[4422:5439,]), round(props[[3]]*100)),] 
  sampl_high =  data[5440:6194,][sample(nrow(data[5440:6194,]), round(props[[2]]*100)),]
  
  
  strat_sample=rbind(sampl_elem,sampl_midd, sampl_high)
  # The svydesign function in the survey package is used to create a survey design object that includes 
  #information about the design and the data. N each strata size, the strata is by stype (school type)
  mydes = svydesign(id=~1, strata = ~stype, data = strat_sample , fpc=~N )
  
  # Inferences can be made by applying special functions to the object created by svydesign
  xbar_EMH[i]<- svymean(~meals, design = mydes)
  # grabs SE from the svymean() function above
  SE_xbar_EMH[i] <- SE(svymean(~meals, design = mydes))
  
  
  }
  
  output<- list (xbar_EMH,mean(xbar_EMH),mean(SE_xbar_EMH))
  names(output) <- c("sample xbar_EMH","mean_xbars_EMH","mean_SE_xbars")
  return(output)
}



stratAPI= strat_sampl_pop(data=apipop_reorder, Nsim=10000, prop=props)


#par(new=TRUE)

plot(density(srsAPI[[1]]),col='blue', ylim=c(0,0.15), xlab = 'subsidized meals percentages', main='Simple Random Sampling')
abline(v=c(srsAPI[[2]]+srsAPI[[3]],srsAPI[[2]]-srsAPI[[3]] ), col=c('blue','blue'),lty=c(2,2))
abline(v=srsAPI[[2]] ,col='blue', lty=1)

#points(density(stratAPI[[1]]), type='l', col='red')
plot(density(stratAPI[[1]]), col='green', xlab = 'subsidized meals percentages', main='Proportional Stratified Sampling')
abline(v=c(stratAPI[[2]]+stratAPI[[3]],stratAPI[[2]]-stratAPI[[3]] ), col=c('green','green'),lty=c(2,2))
abline(v=stratAPI[[2]] ,col='green', lty=1)


