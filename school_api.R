library(survey)
data(api)
head(apipop)
library(corrplot)
install.packages("corrplot")

# First lets look for any correlation!
pairs(apipop[,c("api00","api99","col.grad", "grad.sch", "avg.ed")])
pairs.panels(apipop[,c("api00","api99","col.grad", "grad.sch", "avg.ed")])
M= cor(x=apipop[,c("api00","api99","col.grad", "grad.sch", "avg.ed")], use="pairwise.complete.obs")
corrplot(M,method="number")



##### proportions of schools that are Elementary, Middle school and High school
# since i want to do a proportional stratified sampling using school type as strata

props= table(apipop$stype)/length(apipop$stype)
props

length(apipop$sname[apipop$stype=='E'])
length(apipop$sname[apipop$stype=='M'])
length(apipop$sname[apipop$stype=='H'])

# lets multiply the above proportions by 100 to get an estimate of how many of each school type 
#we should take when doing the sampling 
props*100

elem_data = apipop[apipop$stype=='E',]
midd_data = apipop[apipop$stype=='M',]
high_data = apipop[apipop$stype=='H',]

dataList = list(elem_data, midd_data,high_data)
names(dataList)<- c("ElementarySchoolsData", "MiddleSchoolsData", "HighSchoolsData")

strat_sampl_by_stype<- function(data, Nsim, prop){
  xbar_strat_E <- NA
  xbar_strat_M <- NA
  xbar_strat_H <- NA
  samplSd_E <- NA
  samplSd_M <- NA
  samplSd_H <- NA
for(i in 1:Nsim){sampl_elem= sample(data[[1]]$col.grad, props[[1]]*100)
  sampl_high = sample(data[[3]]$col.grad, props[[2]]*100)
  sampl_midd= sample(data[[2]]$col.grad, props[[3]]*100)
  xbar_strat_E[i]<- mean(sampl_elem)
  xbar_strat_H[i]<- mean(sampl_high)
  xbar_strat_M[i]<- mean(sampl_midd)
  
  
  samplSd_E[i] <- sd(sampl_elem)
  samplSd_M[i] <- sd(sampl_high)
  samplSd_H[i] <- sd(sampl_midd)
}

output<- list (mean(xbar_strat_E), mean(xbar_strat_H),mean(xbar_strat_M),mean(samplSd_E), mean(samplSd_H),mean(samplSd_M) )
names(output) <- c("mean_elem_xbars", "mean_high_xbars", "mean_midd_xbars", "mean_elem_sampl_sd","mean_high_sampl_sd","mean_midd_sampl_sd")
return(output) 
}


strat_sampl_by_stype(data=dataList, Nsim=1000, prop=props)

# compare above to simple statistics for col.grad (percent par.ents with college degree) by stype (school type)
# metrics for apipop by school type (Elementary, middle and high school)
tapply(X=apipop$col.grad, INDEX =apipop$stype, FUN=mean)
tapply(X=apipop$col.grad, INDEX =apipop$stype, FUN=sd)
tapply(X=apipop$col.grad, INDEX =apipop$stype, FUN=var)




###############################################

strat_sampl_pop<- function(data, Nsim, prop){
  xbar_strat_EMH <- NA
  samplSd_EMH <- NA
  samplVar_EMH <- NA
  for(i in 1:Nsim){sampl_elem= sample(data[[1]]$col.grad, round(props[[1]]*100))
  sampl_high = sample(data[[2]]$col.grad, round(props[[2]]*100))
  sampl_midd= sample(data[[3]]$col.grad, round(props[[3]]*100))
  
  xbar_strat_EMH[i]<- mean(c(sampl_elem, sampl_midd,sampl_high))
  samplSd_EMH[i] <- sd(c(sampl_elem,sampl_midd,sampl_high))
  samplVar_EMH[i] <- var(c(sampl_elem,sampl_midd,sampl_high))
  }
  
  output<- list (mean(xbar_strat_EMH),var(xbar_strat_EMH),mean(samplSd_EMH), mean(samplVar_EMH))
  names(output) <- c("mean_EMH_xbars","var_EMH_xbars","mean_EMH_sampl_sd", "mean_EMH_sampl_var")
  return(output) 
}

strat_sampl_pop(data=dataList, Nsim=10000, prop=props)



pop_srs<-function(data, Nsim, prop){
  xbars_pop = NA
  pop_sd = NA
  pop_var = NA
for( i in 1:Nsim){srs=sample(data$col.grad, sum(round(props*100)))
xbars_pop[i] <- mean(srs)
pop_sd[i] <- sd(srs)
pop_var[i] <- var(srs)

}
output <- list(mean(xbars_pop),var(xbars_pop) ,mean(pop_sd),mean(pop_var) )
names(output) <- c("mean_xbars_pop","var_xbars_pop" ,"pop_sd", "pop_var")
return(output)
}

pop_srs(apipop, 10000, props)



# compare metrics for overall population to above
mean(apipop$col.grad)
sd(apipop$col.grad)
var(apipop$col.grad)


############### cluster sampling##################
install.packages("sampling")
library(sampling)
library(MASS)
library(lpSolve)

c=cluster(data=apipop, clustername="dname", size=15, method="srswor",pik,description=FALSE)
d=getdata(apipop, c)

clust_sampling <- function(data, Nsim, clust_variabls, clust_size){
  xbars_pop=NA
  sd_pop =  NA
  var_pop = NA
  for(i in 1:Nsim){
  clust_sampl = cluster(data=data, clustername=clust_variabls, size=clust_size, method="srswor",pik,description=FALSE)
  getSample_data= getdata(data, clust_sampl)
  xbars_pop[i] <- mean(getSample_data$col.grad)
  sd_pop[i]    <- sd(getSample_data$col.grad)
  var_pop[i]   <- var(getSample_data$col.grad)
  }
  output <- list(mean(xbars_pop),var(xbars_pop) ,mean(sd_pop),mean(var_pop) )
  names(output) <- c("mean_xbars_pop","var_xbars_pop" ,"sd_pop", "var_pop")
  return(output)
}


clust_sampling(data= apipop, Nsim = 100, clust_var ="dname", clust_size=15)



#################### two stage clustering sampling############
library(MASS)

reorder_dat=apipop[order(apipop$dname, apipop$sname),]
unique(reorder_dat$dname) # the variable dname (district name) has 757 categories 
unique(reorder_dat$sname) # the variable sname (school name) has 5210 names and its used for the 2nd stage clustering

# 1 cluster (district) is selected in the first stage 
# 10 schools are selected in the second stage from each sampled district
# method is simple random sampling without replacement in each stage
#(equal probability, without replacement)


mstage_sampl=mstage(data=apipop, stage=list("cluster","cluster"), varnames=list("dname","sname"), size=list(1,c(10)),
       method=list("srswor","srswor"), pik, description=FALSE) 
# the first stage is mstage_sampl[[1]], the second stage is mstage_sampl[[2]]

#the selected dsitricts
unique(mstage_sampl[[1]]$dname)
#the selected schools
unique(mstage_sampl[[2]]$sname)
# extracts the observed data
data=getdata(reorder_dat,mstage_sampl)[[2]]
# check the output
table(data$dname,data$sname)


TwoStage_ClustSampl <- function(data,stages_list,vars_list, clustNum_sampleNum, samplMethod_list,Nsim){
  #xbars_col.grad = NA
  for(i in 1:Nsim){
    
    reorder_dat = data[order(data[,grep(vars_list[[1]],colnames(data)) ], data[,grep(vars_list[[2]],colnames(data)) ]),]
    mstage_sampl= mstage(data=data, stage=stages_list, varnames=vars_list, size=clustNum_sampleNum,
                  method=samplMethod_list, pik, description=FALSE) 
    print(mstage_sampl)
    #getdata= getdata(reorder_dat, mstage_sampl)[[2]]
    #xbars_col.grad[i] <- mean(getdata$col.grad) 
  
  }
  #return(xbars_col.grad)
}



TwoStage_ClustSampl(data=apipop,stages_list = list("cluster","cluster"), vars_list = list("dname","sname"),  
                    clustNum_sampleNum = list(1,c(10)), samplMethod_list=list("srswor","srswor"),Nsim=2)


### example using 'swissmunicipalities' data ########


data("swissmunicipalities") # Uses the 'swissmunicipalities' data
b= swissmunicipalities
b= b[order(b$REG, b$CT),]
unique(b$REG) # the variable 'REG' (region) has 7 categories
# it is used as clustering variable in the first-stage sample

unique(b$CT)# the variable 'CT' (canton) has 26 categories;
# it is used as clustering variable in the second-stage sample


# 4 clusters (regions) are selected in the first-stage
# 1 canton is selected in the second-stage from each sampled region
# the method is simple random sampling without replacement in each stage
#(equal probability, without replacement)
m=mstage(b,stage=list("cluster","cluster"), varnames=list("REG","CT"),
         size=list(4,c(1,1,1,1)), method=list("srswor","srswor"))
# the first stage is m[[1]], the second stage is m[[2]]
#the selected regions
unique(m[[1]]$REG)
#the selected cantons
unique(m[[2]]$CT)
# extracts the observed data
x=getdata(b,m)[[2]]
# check the output
table(x$REG,x$CT)


