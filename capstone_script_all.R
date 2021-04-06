## Capstone Forecasting Conflict : The role of street networks in the spatial configuration of the deaths during the Troubles in Belfast

#Contents:

# 1. Regressions: lines 21 - 115
# 2. Spatial Predictions: lines 116-230
# 3. Temporal Predictions: lines 238- 


#Data and libraries
library(stargazer)
library(readxl)
library(MASS)

all_together_now <- read_excel("C:/Users/niamh/Dropbox/Diss/Diss/Crucial DATA/Final/final.xls")
names(all_together_now)
all_together_now$count_lag <- as.numeric(all_together_now$count_lag)
all_together_now$year_fe <- as.factor(all_together_now$year)


############################
# REGRESSIONS
############################

#BASE
#base poisson
base_p <- glm(count ~ population_census + sq_km + distance_bch + count_lag + average_count_neighbours, family=poisson, all_together_now)
summary(base_p)

#base nb
base_nb <- glm.nb(count ~ population_census + sq_km + distance_bch + count_lag + average_count_neighbours, all_together_now)
summary(base_nb)

#LITERATURE
#literature poisson
lit_p <- glm(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
               strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime, family=poisson, all_together_now)
summary(lit_p)

#literature nb
lit_nb <- glm.nb(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                   strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime, all_together_now)
summary(lit_nb)

#MODEL
#my model p
main_p <- glm(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime + integration + meandepth + choice + 
                connectivity + peacewall_late_numeric + average_count_neighbours, family=poisson, all_together_now)
summary(main_p)

#my model nb
main_nb <- glm.nb(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                    strong_religious_majority_dummy + majority_protestant_dummy + integration + meandepth + choice + connectivity + 
                    mean_other_crime + average_count_neighbours + peacewall_late_numeric, all_together_now)
summary(main_nb)

#add fixed time effects
all_together_now$year_fe <- as.factor(all_together_now$year)
head(all_together_now$year_fe)

#my model p with time fe
main_p.time <- glm(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                     strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime + integration + meandepth + choice + 
                     connectivity + peacewall_late_numeric + average_count_neighbours + year_fe, family=poisson, all_together_now)
summary(main_p.time)

#my model nb time fe
main_nb.time <- glm.nb(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                         strong_religious_majority_dummy + majority_protestant_dummy + integration + meandepth + choice + connectivity +
                         mean_other_crime + average_count_neighbours + year_fe + peacewall_late_numeric, all_together_now)
summary(main_nb.time)

#########
#robust with neighbourhood/spatial lag variable included

#without time fe
main_nb_model_notfe_robust <- glm.nb(count ~ integration + meandepth + choice + connectivity +  + peacewall_late_numeric + population_census + 
                                       log_sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                                       majority_protestant_dummy + 
                                       mean_other_crime + average_count_neighbours, all_together_now)

#with time fe
main_nb_model_robust <- glm.nb(count ~ integration + meandepth + choice + connectivity +  + peacewall_late_numeric + population_census + 
                                 log_sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                                 majority_protestant_dummy + 
                                 mean_other_crime + as.factor(year) + average_count_neighbours, all_together_now)


########
#one independent variable at a time

#integration
model_integration <- glm.nb(count ~ integration + population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                             strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime + peacewall_late_numeric + year_fe + average_count_neighbours, 
                           all_together_now)
summary(model_integration)

#mean depth
model_mean_depth <- glm.nb(count ~ meandepth + population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                             strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime + peacewall_late_numeric + year_fe + average_count_neighbours, 
                           all_together_now)
summary(model_mean_depth)

#choice
model_choice <- glm.nb(count ~ choice + population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                         strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime + peacewall_late_numeric + year_fe + average_count_neighbours, 
                       all_together_now)
summary(model_choice)

#connectivity
model_connectivity <- glm.nb(count ~ connectivity +population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                               strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime + peacewall_late_numeric + year_fe + average_count_neighbours,
                             all_together_now)
summary(model_connectivity)


############################
# SPATIAL PREDICTIONS
############################

############################
######### Initial Prediction
mydata <- read_excel("C:/Users/niamh/Dropbox/Diss/Diss/Crucial DATA/Final/final.xls")
mydata$year_fe <- as.factor(mydata$year)

randomSampleofWards <- sample(mydata$wardname, size = 50, replace=T)

spacelearningSetx <- mydata[mydata$wardname %in% randomSampleofWards,]
spacetestSetx <- mydata[!mydata$wardname %in% randomSampleofWards,]

mydata$predictions.main.spatial.nb <- NA
mydata$predictions.lit.spatial.nb <- NA
mydata$predictions.base.spatial.nb <- NA


#run the regressions on the Learning Set - NEGATIVE BINOMIAL
#base



for(i in randomSampleofWards){
  print(i)
  base_glm_spatial_nb <- glm.nb(count ~ population_census + sq_km + distance_bch + count_lag, spacelearningSetx, na.action= na.exclude)
  
  #lit
  lit_glm_spatial_nb <- glm.nb(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                                 strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime + peacewall_late_dummy, spacelearningSetx)
  
  #main
  main_glm_spatial_nb <- glm.nb(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                                  strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime + integration + meandepth + 
                                  connectivity + choice + peacewall_late_dummy, spacelearningSetx)
  #Use coefficients to predict on test set (rolling.year)
  mydata$predictions.main.spatial.nb[mydata$wardname == i] <- as.numeric(predict(main_glm_spatial_nb, newdata = mydata[mydata$wardname == i,], type='response'))
  mydata$predictions.lit.spatial.nb[mydata$wardname == i] <- as.numeric(predict(lit_glm_spatial_nb, newdata = mydata[mydata$wardname == i,], type='response'))
  mydata$predictions.base.spatial.nb[mydata$wardname == i] <- as.numeric(predict(base_glm_spatial_nb, newdata = mydata[mydata$wardname == i,], type='response'))
}

mse.main.spatial.nb <- mean((mydata$count - mydata$predictions.main.spatial.nb)^2, na.rm=T)
mse.lit.spatial.nb <- mean((mydata$count - mydata$predictions.lit.spatial.nb)^2, na.rm=T)
mse.base.spatial.nb <- mean((mydata$count - mydata$predictions.base.spatial.nb)^2, na.rm=T)

############################
######### Function for bootstrap

runPredictionsX <- function(thisdata){
  randomSampleofWards <- sample(thisdata$wardname, size = 50, replace=T)
  
  spacelearningSetx <- thisdata[thisdata$wardname %in% randomSampleofWards,]
  spacetestSetx <- thisdata[!thisdata$wardname %in% randomSampleofWards,]
  
  thisdata$predictions.main.spatial.nb <- NA
  thisdata$predictions.lit.spatial.nb <- NA
  thisdata$predictions.base.spatial.nb <- NA
  
  
  #run the regressions on the Learning Set - NEGATIVE BINOMIAL
  #base
  
  
  
  for(i in randomSampleofWards){
    print(i)
    base_glm_spatial_nb <- glm.nb(count ~ population_census + sq_km + distance_bch + count_lag + year_fe, spacelearningSetx)
    
    #lit
    lit_glm_spatial_nb <- glm.nb(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                                   strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime + year_fe, spacelearningSetx)
    
    #main
    main_glm_spatial_nb <- glm.nb(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                                    strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime + integration + meandepth + 
                                    connectivity + year_fe, spacelearningSetx)
    
    #Use coefficients to predict on test set (rolling.year)
    thisdata$predictions.main.spatial.nb[thisdata$wardname == i] <- as.numeric(predict(main_glm_spatial_nb, newdata = thisdata[thisdata$wardname == i,], type='response'))
    thisdata$predictions.lit.spatial.nb[thisdata$wardname == i] <- as.numeric(predict(lit_glm_spatial_nb, newdata = thisdata[thisdata$wardname == i,], type='response'))
    thisdata$predictions.base.spatial.nb[thisdata$wardname == i] <- as.numeric(predict(base_glm_spatial_nb, newdata = thisdata[thisdata$wardname == i,], type='response'))
  }
  
  mse.main.spatial.nb <- mean((thisdata$predictions.main.spatial.nb - thisdata$count)^2, na.rm=T)
  mse.lit.spatial.nb <- mean((thisdata$predictions.lit.spatial.nb - thisdata$count)^2, na.rm=T)
  mse.base.spatial.nb <- mean((thisdata$predictions.base.spatial.nb - thisdata$count)^2, na.rm=T)
  
  return(c(mse.main.spatial.nb, mse.lit.spatial.nb, mse.base.spatial.nb))
  
}

############################
######### Bootstrapping


MSEs.literaturemodelnb <- NULL
MSEs.mymodelnb <- NULL
MSEs.basenb <- NULL

for(i in 1:500){ # Loop: create a `new' data 1000 times and calculate the AUC for each
  # pb$tick()
  # sample observations numbers from 1 to N, and
  # pick N of them (with replacement)
  sampleObs <- sample(1:nrow(mydata), size=nrow(mydata), replace=T)
  
  # create a new dataset with data from the sample observations
  alternate.world.data <- mydata[sampleObs, ]
  
  # run the predictions function defined above on that new data
  this.auc <- runPredictionsX(alternate.world.data)
  MSEs.literaturemodelnb <- c(MSEs.literaturemodelnb, this.auc[2])
  MSEs.mymodelnb <- c(MSEs.mymodelnb, this.auc[1])
  MSEs.basenb <- c(MSEs.basenb, this.auc[3])
}
summary(MSEs.literaturemodelnb)
summary(MSEs.mymodelnb)
summary(MSEs.basenb)

boxplot(MSEs.mymodelnb, MSEs.literaturemodelnb, MSEs.basenb, names = c('My Model', 'Literature Model', 'Base'))

t.test(MSEs.mymodelnb, MSEs.literaturemodelnb)


############################
# TEMPORAL PREDICTIONS
############################

############################
######### Initial Prediction
#Initialize variables to store predictions
  mydata$predictions.main.p <- NA
  mydata$predictions.lit.p <- NA
  mydata$predictions.base.p <- NA
  
  learningSet <- mydata[mydata$year < rolling.year,]
  testSet <- mydata[mydata$year == rolling.year,]
  
  #Run regressions on learning set  
  glm.base.p <- glm(count ~ population_census + sq_km + distance_bch + count_lag, family=poisson, data = learningSet)
  
  glm.lit.p <- glm(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                     strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime, family=poisson, data = learningSet)
  
  glm.main.p <- glm(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                      strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime + integration + meandepth + choice + 
                      connectivity + peacewall_late_numeric, family=poisson, data = learningSet)
  
  
  
  
  #Rolling window predictions for each model
  
  for(rolling.year in 1972:2001){
    print(rolling.year)
    #Define learning and test sets
    
    #Use coefficients to predict on test set (rolling.year)
    mydata$predictions.main.p[mydata$year == rolling.year] <- as.numeric(predict(glm.main.p, newdata = testSet, type='response'))
    mydata$predictions.lit.p[mydata$year == rolling.year] <- as.numeric(predict(glm.lit.p, newdata = testSet, type='response'))
    mydata$predictions.base.p[mydata$year == rolling.year] <- as.numeric(predict(glm.base.p, newdata = testSet, type='response'))
  }
  
  #Calculate MSE
  mse.main <- (mean((mydata$predictions.main.p - mydata$count)^2, na.rm=T))
  mse.lit <- (mean((mydata$predictions.lit.p - mydata$count)^2, na.rm=T))
  mse.base <- (mean((mydata$predictions.base.p - mydata$count)^2, na.rm=T))
  

############################
######### Function for bootstrap

runPredictionstemp <- function(thisdata){
  #Initialize variables to store predictions
  thisdata$predictions.main.p <- NA
  thisdata$predictions.lit.p <- NA
  thisdata$predictions.base.p <- NA
  
  learningSet <- thisdata[thisdata$year < rolling.year,]
  testSet <- thisdata[thisdata$year == rolling.year,]
  
  #Run regressions on learning set  
  glm.base.p <- glm(count ~ population_census + sq_km + distance_bch + count_lag, family=poisson, data = learningSet)
  
  glm.lit.p <- glm(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                     strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime, family=poisson, data = learningSet)
  
  glm.main.p <- glm(count ~ population_census + sq_km + distance_bch + count_lag + mean_unemp_pc + housing_claimants_pc +
                      strong_religious_majority_dummy + majority_protestant_dummy + mean_other_crime + integration + meandepth + choice + 
                      connectivity + peacewall_late_numeric, family=poisson, data = learningSet)
  
  
  
  
  #Rolling window predictions for each model
  
  for(rolling.year in 1972:2001){
    print(rolling.year)
    #Define learning and test sets
    
    #Use coefficients to predict on test set (rolling.year)
    thisdata$predictions.main.p[thisdata$year == rolling.year] <- as.numeric(predict(glm.main.p, newdata = testSet, type='response'))
    thisdata$predictions.lit.p[thisdata$year == rolling.year] <- as.numeric(predict(glm.lit.p, newdata = testSet, type='response'))
    thisdata$predictions.base.p[thisdata$year == rolling.year] <- as.numeric(predict(glm.base.p, newdata = testSet, type='response'))
  }
  
  #Calculate MSE
  mse.main <- (mean((thisdata$predictions.main.p - thisdata$count)^2, na.rm=T))
  mse.lit <- (mean((thisdata$predictions.lit.p - thisdata$count)^2, na.rm=T))
  mse.base <- (mean((thisdata$predictions.base.p - thisdata$count)^2, na.rm=T))
  
  return(c(mse.main, mse.lit, mse.base))
  
}

############################
######### Bootstrapping

  MSEs.mymodel <- NULL
  MSEs.base <- NULL
  MSEs.literaturemodel <- NULL
  
  for(i in 1:500){ # Loop: create a `new' data 1000 times and calculate the AUC for each
    #pb$tick()
    # sample observations numbers from 1 to N, and
    # pick N of them (with replacement)
    sampleObs <- sample(1:nrow(mydata), size=nrow(mydata), replace=T)
    
    # create a new data set with data from the sample observations
    alternate.world.data <- mydata[sampleObs, ]
    
    # run the predictions function defined above on that new data
    this.auc <- runPredictionstemp(alternate.world.data)
    MSEs.literaturemodel <- c(MSEs.literaturemodel, this.auc[2])
    MSEs.mymodel <- c(MSEs.mymodel, this.auc[1])
    MSEs.base <- c(MSEs.base, this.auc[3])
  } 
  
  
  summary(MSEs.mymodel)
  summary(MSEs.literaturemodel)
  summary(MSEs.base)