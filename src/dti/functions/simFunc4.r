#####################################################################
## Functions to generate the data for all the models for the study ##
#####################################################################

##function to check if model constraints are met
param.check.constraints <- function(pprobI) {
    
    ppImon <- pprobI[,order(pprobI[1,])]
    mon <- !any(apply(ppImon, 2, is.unsorted))
    
    ppIiio <- pprobI[order(pprobI[,1]),]
    iio <- !any(apply(ppIiio, 1, is.unsorted))
        
    return(c(mon, iio))
    
}

plot.LParam <- function(x,scale = "logit"){
    
    matplot( t(x[[scale]]), type = "l", ylab = scale, xlab = "Items", col = c('red','dodgerblue','darkgreen'))
    title( paste( "Model ", x["model"]))
    
}

# nC = 6 
# nI = 6 
# model = 1

sim.LParam <- function ( nC = 6, nI = 6, model = 0, logit = NULL, lC = NULL, lI = NULL) {
        
        if ( model > 5 ) { return(print('Error: Invalid Model.')) }
        
        if ( !is.null( lC ) ) { nC <- length( lC ) }
        if ( !is.null( lI ) ) { nI <- length( lI ) }
        
        output <- list()
        
        constraints.check.pending = TRUE
        nIter = 0

    while (constraints.check.pending){
        nIter = nIter+1; print(nIter)

        output["model"] <- model        
        output["nC"]    <- nC
        output["nI"]    <- nI
        
        if ( model == 5 ) {
        
            # If Rasch Model then nC stands for N of persons and then is fixed at 1
        
            nR              <- nC
            output["nR"]    <- nR
            output["nC"]    <- 1
        
        
            if ( is.null( lC ) ) { lC <- rnorm( nC ) }
            if ( is.null( lI ) ) { lI <- runif( nI , min = -4, max = 4) }
                        
            lC <- sort(lC)
            lI <- sort(lI)
            
            logit <- matrix( rep( sort(-lI), nR ), ncol = nI, byrow = TRUE)
            logit <- apply( logit, 2, '+', lC)
            
            output[["lC"]] <- lC
            output[["lI"]] <- lI    
        
        }
        
        if ( model == 4 ) {
            
            if ( is.null( lC ) ) { 
                
                sep <- TRUE
                
                while(sep) {
                
                    lC <- runif( nC , min = -4, max = 4 ) 
                
                    sep <- any(0 < abs(kronecker(lC, lC, FUN = '-')) & abs(kronecker(lC, lC, FUN = '-')) < 0.5)
                    
                }
                
            }
                
            if ( is.null( lI ) ) { lI <- runif( nI , min = -5, max = 5 ) }
            
            lC <- sort(lC)
            lI <- sort(lI)
            
            logit <- matrix( rep( sort(-lI), nC ), ncol = nI, byrow = TRUE)
            logit <- apply( logit, 2, '+', lC)
            
            output[["lC"]] <- lC
            output[["lI"]] <- lI
            
        }
        
        if ( model <  4 & is.null( logit ) ) { logit <- matrix( runif( nC * nI , min = -5, max = 5 ), ncol = nI ) }

        if ( model == 3 ) {
            
            logit <-    apply( logit, 2, sort)
            logit <- t( apply( logit, 1, sort))
            
        }

        if ( model == 2 ) { logit <- t( apply( logit, 1, sort)) }

        if ( model == 1 ) { logit <- apply( logit, 2, sort) }
        
        prob  <- ( 1 / ( 1 + exp( - logit ) ) )

        # Checking that only the model specified constraints are being fulfilled.

        if (model == 0 & !any(param.check.constraints(prob))) {

            constraints.check.pending = FALSE

        }

        if (model == 1 & ( param.check.constraints(prob)[1] & !param.check.constraints(prob)[2] )){

            constraints.check.pending = FALSE

        }

        if (model == 2 & ( !param.check.constraints(prob)[1] & param.check.constraints(prob)[2] )){

            constraints.check.pending = FALSE

        }

        if (model > 2 & ( param.check.constraints(prob)[1] & param.check.constraints(prob)[2] )){

            constraints.check.pending = FALSE

        }

    }

    colnames(logit) <- paste( "i", seq( 1:ncol( logit) ), sep = '' )
    colnames(prob ) <- paste( "i", seq( 1:ncol( logit) ), sep = '' )

    rownames(logit) <- paste( "c", seq( 1:nrow( logit) ), sep = '' )
    rownames(prob ) <- paste( "c", seq( 1:nrow( logit) ), sep = '' )


    output[["logit"]] <- logit
    output[["prob"]]  <- prob
    
    return(output)

}

# test <- sim.LParam( nC = 6, nI = 6, model = 3)
# param.check.constraints(test$prob)
# plot.LParam(test)


# LPar <- test
# nR < - 500
# prRespC <- rep(1/6, 6)

# prItemC <- rep(1/6, 6)
# nI <- 30


 LPar = model.LParam
 prRespC = prRespC
 nR = nR
 prItemC = prItemC
 nI = nI


sim.OResp2 <- function ( LPar, prRespC = NULL, nR = NULL, prItemC = NULL, nI = NULL){

    output <- list()

    if ( LPar["model"] < 5){

        cR      <- rep( seq( 1: length( prRespC) ) , rmultinom( 1, nR, prRespC) )
        cR      <- sample( cR, size = nR)

        cI      <- rep( seq( 1: length( prItemC) ) , rmultinom( 1, nI, prItemC) )
        cI      <- sample( cI, size = nI)

        prRI    <- LPar[["prob"]][ cR, cI]

        genD    <- cbind( cR, prRI)

    }

    if ( LPar["model"] == 5){
    
        nR      <- LPar[["nR"]]
        prRI    <- LPar[["prob"]]
        genD    <- prRI
    
    }

    respRI  <- matrix( sapply( c(prRI), rbinom, n = 1, size = 1), ncol = ncol(prRI))
    
    obsD    <- respRI
    
    colnames(obsD)      <- c( paste( "i", seq( 1:ncol( obsD) ),sep = "" ) )

    output[["obsData"]] <- obsD
    output[["genData"]] <- genD 
    output["model"]     <- LPar["model"]
    output["nrespC"]    <- LPar["nC"]

    output["nitemC"]    <- LPar["nI"]

    
    if ( LPar["model"] < 5){

        output["nR"]        <- nR
        output["nI"]        <- nI

        output[["cR"]]       <- table(cR)
        output[["prRespC"]]  <- prRespC    

        output[["cI"]]       <- table(cI)
        output[["prItemC"]]  <- prItemC
    
    }

    output[["logit"]]   <- LPar[["logit"]]
    output[["prob"]]    <- LPar[["prob"]]

    if ( LPar["model"] >= 4){
        
        output[["lC"]] <- LPar[["lC"]]
        output[["lI"]] <- LPar[["lI"]]
        
    }

    return(output)

}
    



# test <- sim.LParam( nC = 6, nI = 6, model = 4)
# param.check.constraints(test$prob)
# plot.LParam(test)


# LPar <- test
# nR < - 500
# prC <- rep(1/6, 6)


# sim.OResp(test, prC, nR)


