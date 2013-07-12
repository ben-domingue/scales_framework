#########################################################
## Code to Generate (90) Datasets for Simulation Study ##
#########################################################

## set working directory (change this!!!)
setwd("/Users/dti/Dropbox/Research/TACM/src/dti")


## source data generation / write functions
source("./functions/simFunc4.r")
source("./functions/wrtFunc2.r")


## set fixed simulation conditions
# number of persons
nR = 500

# number of items
nI = 30

# number of models
nM = 300


## 'obtain' varying simulation conditions
# number of each model
# mod01 <- rmultinom(n = 1, size = nM, prob = rep(1/6, 6))
# mod02 <- rep(0:5, times = mod01)
# mod03 <- sample(mod02)
# table(mod03); print("If you don't like the distribution, tweak it!")

mod03 <- rep( 0:5, each = 50)

# number of classes for each model
#nC01 <- sample(2:6, size = nM, replace = TRUE, prob = c(1, 2, 2, 2, 1))

# model selection
models <- data.frame(model = mod03, nC = 6)
models[models$model==5,'nC'] <- nR
table(models$model, models$nC); print("If you don't like the distribution, tweak it!")

## generate the data!

## set working directory (change this!!!)
setwd("/Users/dti/Dropbox/Research/TACM/src/dti/simdata")   #David

for(i in 1:nM) {
    
    # pull nModel and nClasses, calc class probs 
    model <- models[i, 'model']
    # nC <- 6
    prRespC <- rep(1/6, 6)
    prItemC <- rep(1/6, 6)

    # generate the data 
    model.LParam <- sim.LParam(nC = 6, nI = 6, model = model)
    model.LOResp <- sim.OResp2(LPar = model.LParam, prRespC = prRespC, nR = 500, prItemC = prItemC, nI = nI)
    # filename(s)? 
#    filestem = paste("D", i, sep = "")  #David
#    filestem = paste("R", i, sep = "")  #Ronli
    filestem = paste("T", i, sep = "")  #Ronli


    svfname = paste(filestem, '.Rdata', sep = '')
    
    # write data for LG (includes writing info file) 
    write.lg(model.LOResp, filestem)
    
    # write data for VerProg 
#    write.ver.dat(model.LOResp, filestem)
    
    # save data as R object 
    save(model.LOResp, file = svfname)
    
}




