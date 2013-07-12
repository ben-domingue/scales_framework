######################################################################
## Functions to write the data and input files for various programs ##
######################################################################

##function to write data files for LatentGold
write.lg <- function(LCo, filestem) {
    
    info <- paste(filestem,"- Info.txt")
    data <- paste(filestem,"_lg.dat", sep = '')
    
    sink(info)
    print(paste("The model used to generate the data was",LCo["model"]))
    print(paste("The model included",LCo["nC"], "classes and ",LCo["nI"], "items"))
    print(paste("The simulated dataset contained",LCo["nR"],"respondents"))
    print(paste("The proportions assigned to the classes are",paste(LCo[["prC"]],collapse=", ")))
    print(paste("The respondents were assigned to the classes with frequencies",paste(LCo[["cR"]],collapse=", ")))
    print("")
    print("The generating parameters (as logits) are:")
    print(round(t(LCo[["logit"]]), 2))
    print("")
    print("The generating parameters (as Pr[x_ri]) are:")
    print(round(t(LCo[["prob"]] ), 2))
    print("")
    
    if ( LCo[["model"]] == 4 ){
        
        print("The class locations are:")
        print(round(LCo[["lC"]],2))

        print("")

        print("The item locations are:")
        print( matrix( round( LCo[["lI"]], 2), ncol = 10 ) )

    }
    
    
    sink()

    write.table(LCo[["obsData"]], file = data, 
    sep="\t", na = ".", row.names = FALSE, quote = FALSE, col.names=TRUE)

}

##function to write a syntax file for Latent Gold
##Notice that data.file must contain the full path to the datafile
write.lg.lgs <- function(model = 0, nC, nI, data.file, lgs.file) {

    cat('//LG4.5//'                                                                         ,'\n', file = lgs.file, append = F)
    cat('version = 4.5'                                                                     ,'\n', file = lgs.file, append = T)
    cat(paste("infile ", "'", data.file, "'", sep="")                                       ,'\n', file = lgs.file, append = T)
    cat(' '                                                                                 ,'\n', file = lgs.file, append = T)
    cat('model'                                                                             ,'\n', file = lgs.file, append = T)
    cat('options'                                                                           ,'\n', file = lgs.file, append = T)
    cat(' '                                                                                 ,'\n', file = lgs.file, append = T)
    cat('       algorithm'                                                                  ,'\n', file = lgs.file, append = T)
    cat('           tolerance=1e-008 emtolerance=0.01 emiterations=500 nriterations=100;'   ,'\n', file = lgs.file, append = T)
    cat(' '                                                                                 ,'\n', file = lgs.file, append = T)
    cat('       startvalues'                                                                ,'\n', file = lgs.file, append = T)
    cat('           seed=0 sets=50 tolerance=1e-005 iterations=50;'                         ,'\n', file = lgs.file, append = T)
    cat(' '                                                                                 ,'\n', file = lgs.file, append = T)
    cat('       bayes'                                                                      ,'\n', file = lgs.file, append = T)
    cat('           categorical=1 variances=1 latent=1 poisson=1;'                          ,'\n', file = lgs.file, append = T)
    cat(' '                                                                                 ,'\n', file = lgs.file, append = T)

    if ( model == 5 ){

        cat('       quadrature  nodes=10;'                                                  ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)

    }

    cat('       missing'                                                                    ,'\n', file = lgs.file, append = T)
    cat('           excludeall;'                                                            ,'\n', file = lgs.file, append = T)
    cat(' '                                                                                 ,'\n', file = lgs.file, append = T)
    cat('       output'                                                                     ,'\n', file = lgs.file, append = T)
    cat('           parameters=first standarderrors probmeans=posterior'                    ,'\n', file = lgs.file, append = T)
    cat('           profile bivariateresiduals frequencies classification'                  ,'\n', file = lgs.file, append = T)
    cat('           estimatedvalues;'                                                       ,'\n', file = lgs.file, append = T)
    cat(' '                                                                                 ,'\n', file = lgs.file, append = T)
    cat('variables'                                                                         ,'\n', file = lgs.file, append = T)
    cat(' '                                                                                 ,'\n', file = lgs.file, append = T)
    cat(  paste( '//ModelType', model)       					                                    ,'\n', file = lgs.file, append = T)
    cat(  paste( '//NClasses', nC)       					                                    ,'\n', file = lgs.file, append = T)
    cat(  paste( '//NItems', nI)       					                                    ,'\n', file = lgs.file, append = T)
    cat(' '                                                                                 ,'\n', file = lgs.file, append = T)
    cat('       dependent'                                                                  ,'\n', file = lgs.file, append = T)
    cat( paste( paste('i', 1:nI, sep = '', collapse = ', '), ';')                           ,'\n', file = lgs.file, append = T)
    cat(' '                                                                                 ,'\n', file = lgs.file, append = T)

    if ( model == 0 ){

        cat('       latent'                                                                 ,'\n', file = lgs.file, append = T)
        cat( paste( 'LatVariable nominal', nC, ';')                                         ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)
        cat('       equations'                                                              ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)
        cat('           LatVariable  <- 1;'                                                 ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)

        cat( paste( paste('i', 1:nI, sep = '', collapse = ' '), '<- 1 + LatVariable;')      ,'\n', file = lgs.file, append = T)

    }

    if ( model == 1 ){

        cat('       latent'                                                                 ,'\n', file = lgs.file, append = T)
        cat( paste( 'LatVariable nominal', nC, ';')                                         ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)
        cat('       equations'                                                              ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)
        cat('           LatVariable  <- 1;'                                                 ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)

        cat( paste( paste('i', 1:nI, sep = '', collapse = ' '), '<- 1 + (+) LatVariable;')  ,'\n', file = lgs.file, append = T)

    }

    if ( model == 4 ){

        cat('       latent'                                                                 ,'\n', file = lgs.file, append = T)
        cat( paste( 'LatClass nominal', nC, ',')                                                    ,'\n', file = lgs.file, append = T)
        cat('           LatCont continuous ;'                                               ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)
        cat('       equations'                                                              ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)
        cat('           LatClass  <- (a) 1 ;'                                               ,'\n', file = lgs.file, append = T)
        cat('           LatCont   <- (b) 1 | LatClass ;'                                    ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)
        cat('           (0) LatCont ;'                                                      ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)

        cat( paste( paste('i', 1:nI, sep = '', collapse = ' '), '<- 1 + (1) LatCont;')      ,'\n', file = lgs.file, append = T)

        cat(' '                                                                             ,'\n', file = lgs.file, append = T)
        cat('           a    = 0 ;'                                                         ,'\n', file = lgs.file, append = T)
        cat('           b[1] = 0 ;'                                                         ,'\n', file = lgs.file, append = T)

    }

    if ( model == 5 ){

        cat('       latent'                                                                 ,'\n', file = lgs.file, append = T)
        cat('           LatCont continuous ;'                                               ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)
        cat('       equations'                                                              ,'\n', file = lgs.file, append = T)
        cat(' '                                                                             ,'\n', file = lgs.file, append = T)
        cat('           LatCont  ;'                                                         ,'\n', file = lgs.file, append = T)

        cat( paste( paste('i', 1:nI, sep = '', collapse = ' '), '<- 1 + (1) LatCont;')      ,'\n', file = lgs.file, append = T)
        
    }

    cat(' '                                                                                 ,'\n', file = lgs.file, append = T)
    cat('end model'                                                                         ,'\n', file = lgs.file, append = T)

}

##function to write a data file for Vermunt's IIO .exe
write.ver.dat <- function(LCo, filestem) {
    
    data.file <- paste(filestem,"_ver.dat", sep = '')
    
    write.table(LCo[["obsData"]], file = data.file, sep="\t", 
        na = ".", row.names = FALSE, quote = FALSE, col.names=FALSE)

}

##function to write an input file for Vermunt's IIO .exe
write.ver.inp <- function(model = 0, nC, nR, nI, data.file, inp.file) {
    
#    inp.file <- paste(filestem,"ver.inp", sep = '')
#    data.file <- paste(filestem,"ver.dat", sep = '')
    
    cat("tab ", nR, nI, " \n", file = inp.file, append = F)

    cat("cla", nC, " \n", file = inp.file, append = T)

    cat("ite 10000 50000 10 \n", file = inp.file, append = T)

    cat("dat", data.file, " \n", file = inp.file, append = T)

    if (model == 0 | model == 2) { mon <- paste(rep(0, nI), collapse = " ") }
    if (model == 1 | model == 3) { mon <- paste(rep(1, nI), collapse = " ") }

    if (model <= 1) { iio <- paste(rep(0, nI), collapse = " ") }
    if (model >= 2) { iio <- paste(rep(1, nI), collapse = " ") }

    cat("mon", mon, " \n", file = inp.file, append = T)
    
    cat("iio", iio, " \n", file = inp.file, append = T)
   
}
