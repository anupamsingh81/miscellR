plotPost = function( paramSampleVec , credMass=0.95 , compVal=NULL ,
                     HDItextPlace=0.7 , ROPE=NULL , yaxt=NULL , ylab=NULL ,
                     xlab=NULL , cex.lab=NULL , cex=NULL , xlim=NULL , main=NULL ,
                     col=NULL , border=NULL , showMode=F , showCurve=F , breaks=NULL , 
                     ... ) {
  # Override defaults of hist function, if not specified by user:
  # (additional arguments "..." are passed to the hist function)
  if ( is.null(xlab) ) xlab="Parameter"
  if ( is.null(cex.lab) ) cex.lab=1.5
  if ( is.null(cex) ) cex=1.4
  if ( is.null(xlim) ) xlim=range( c( compVal , paramSampleVec ) )
  if ( is.null(main) ) main=""
  if ( is.null(yaxt) ) yaxt="n"
  if ( is.null(ylab) ) ylab=""
  if ( is.null(col) ) col="skyblue"
  if ( is.null(border) ) border="white"
  
  postSummary = matrix( NA , nrow=1 , ncol=11 , 
                        dimnames=list( c( xlab ) , 
                                       c("mean","median","mode",
                                         "hdiMass","hdiLow","hdiHigh",
                                         "compVal","pcGTcompVal",
                                         "ROPElow","ROPEhigh","pcInROPE")))              
  postSummary[,"mean"] = mean(paramSampleVec)
  postSummary[,"median"] = median(paramSampleVec)
  mcmcDensity = density(paramSampleVec)
  postSummary[,"mode"] = mcmcDensity$x[which.max(mcmcDensity$y)]
  
  
  HDI = HDIofMCMC( paramSampleVec , credMass )
  postSummary[,"hdiMass"]=credMass
  postSummary[,"hdiLow"]=HDI[1]
  postSummary[,"hdiHigh"]=HDI[2]
  
  # Plot histogram.
  if ( is.null(breaks) ) {
    breaks = c( seq( from=min(paramSampleVec) , to=max(paramSampleVec) ,
                     by=(HDI[2]-HDI[1])/18 ) , max(paramSampleVec) )
  }
  if ( !showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , xlab=xlab , yaxt=yaxt , ylab=ylab ,
                     freq=F , border=border , col=col ,
                     xlim=xlim , main=main , cex=cex , cex.lab=cex.lab ,
                     breaks=breaks , ... )
  }
  if ( showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , plot=F )
    densCurve = density( paramSampleVec , adjust=2 )
    plot( densCurve$x , densCurve$y , type="l" , lwd=5 , col=col , bty="n" ,
          xlim=xlim , xlab=xlab , yaxt=yaxt , ylab=ylab ,
          main=main , cex=cex , cex.lab=cex.lab , ... )
  }
  cenTendHt = 0.9*max(histinfo$density)
  cvHt = 0.7*max(histinfo$density)
  ROPEtextHt = 0.55*max(histinfo$density)
  # Display mean or mode:
  if ( showMode==F ) {
    meanParam = mean( paramSampleVec )
    text( meanParam , cenTendHt ,
          bquote(mean==.(signif(meanParam,3))) , adj=c(.5,0) , cex=cex )
  } else {
    dres = density( paramSampleVec )
    modeParam = dres$x[which.max(dres$y)]
    text( modeParam , cenTendHt ,
          bquote(mode==.(signif(modeParam,3))) , adj=c(.5,0) , cex=cex )
  }
  # Display the comparison value.
  if ( !is.null( compVal ) ) {
    cvCol = "darkgreen"
    pcgtCompVal = round( 100 * sum( paramSampleVec > compVal )
                         / length( paramSampleVec )  , 1 )
    pcltCompVal = 100 - pcgtCompVal
    lines( c(compVal,compVal) , c(0.96*cvHt,0) ,
           lty="dotted" , lwd=1 , col=cvCol )
    text( compVal , cvHt ,
          bquote( .(pcltCompVal)*"% < " *
                   .(signif(compVal,3)) * " < "*.(pcgtCompVal)*"%" ) ,
          adj=c(pcltCompVal/100,0) , cex=0.8*cex , col=cvCol )
    postSummary[,"compVal"] = compVal
    postSummary[,"pcGTcompVal"] = ( sum( paramSampleVec > compVal ) 
                                    / length( paramSampleVec ) )
  }
  # Display the ROPE.
  if ( !is.null( ROPE ) ) {
    ropeCol = "darkred"
    pcInROPE = ( sum( paramSampleVec > ROPE[1] & paramSampleVec < ROPE[2] )
                 / length( paramSampleVec ) )
    lines( c(ROPE[1],ROPE[1]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol )
    lines( c(ROPE[2],ROPE[2]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol)
    text( mean(ROPE) , ROPEtextHt ,
          bquote( .(round(100*pcInROPE))*"% in ROPE" ) ,
          adj=c(.5,0) , cex=1 , col=ropeCol )
    
    postSummary[,"ROPElow"]=ROPE[1] 
    postSummary[,"ROPEhigh"]=ROPE[2] 
    postSummary[,"pcInROPE"]=pcInROPE
  }
  # Display the HDI.
  lines( HDI , c(0,0) , lwd=4 )
  text( mean(HDI) , 0 , bquote(.(100*credMass) * "% HDI" ) ,
        adj=c(.5,-1.7) , cex=cex )
  text( HDI[1] , 0 , bquote(.(signif(HDI[1],3))) ,
        adj=c(HDItextPlace,-0.5) , cex=cex )
  text( HDI[2] , 0 , bquote(.(signif(HDI[2],3))) ,
        adj=c(1.0-HDItextPlace,-0.5) , cex=cex )
  par(xpd=F)
  #
  return( postSummary )
}
# openGraphSaveGraph.R
# John K. Kruschke, 2013

openGraph = function( width=7 , height=7 , mag=1.0 , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    X11( width=width*mag , height=height*mag , type="cairo" , ... )
  } else { # Windows OS
    windows( width=width*mag , height=height*mag , ... )
  }
}

saveGraph = function( file="saveGraphOutput" , type="pdf" , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    if ( any( type == c("png","jpeg","jpg","tiff","bmp")) ) {
      sptype = type
      if ( type == "jpg" ) { sptype = "jpeg" }
      savePlot( file=paste(file,".",type,sep="") , type=sptype , ... )     
    }
    if ( type == "pdf" ) {
      dev.copy2pdf(file=paste(file,".",type,sep="") , ... )
    }
    if ( type == "eps" ) {
      dev.copy2eps(file=paste(file,".",type,sep="") , ... )
    }
  } else { # Windows OS
    savePlot( file=file , type=type , ... )
  }
}
HDIofICDF = function( ICDFname , credMass=0.95 , tol=1e-8 , ... ) {
  # Arguments:
  #   ICDFname is R's name for the inverse cumulative density function
  #     of the distribution.
  #   credMass is the desired mass of the HDI region.
  #   tol is passed to R's optimize function.
  # Return value:
  #   Highest density iterval (HDI) limits in a vector.
  # Example of use: For determining HDI of a beta(30,12) distribution, type
  #   HDIofICDF( qbeta , shape1 = 30 , shape2 = 12 )
  #   Notice that the parameters of the ICDFname must be explicitly named;
  #   e.g., HDIofICDF( qbeta , 30 , 12 ) does not work.
  # Adapted and corrected from Greg Snow's TeachingDemos package.
  incredMass =  1.0 - credMass
  intervalWidth = function( lowTailPr , ICDFname , credMass , ... ) {
    ICDFname( credMass + lowTailPr , ... ) - ICDFname( lowTailPr , ... )
  }
  optInfo = optimize( intervalWidth , c( 0 , incredMass ) , ICDFname=ICDFname ,
                      credMass=credMass , tol=tol , ... )
  HDIlowTailPr = optInfo$minimum
  return( c( ICDFname( HDIlowTailPr , ... ) ,
             ICDFname( credMass + HDIlowTailPr , ... ) ) )
}                  # Kruschke, J. K. (2011). Doing Bayesian data analysis: A
# Tutorial with R and BUGS. Elsevier Science/Academic Press.
HDIofMCMC = function( sampleVec , credMass=0.95 ) {
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sampleVec
  #     is a vector of representative values from a probability distribution.
  #   credMass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = floor( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}
BESTmcmc = function( y1, y2, numSavedSteps=20000, thinSteps=1, showMCMC=FALSE) { 
  # This function generates an MCMC sample from the posterior distribution.
  # Description of arguments:
  # showMCMC is a flag for displaying diagnostic graphs of the chains.
  #    If F (the default), no chain graphs are displayed. If T, they are.
  
  require(rjags)
  
  #------------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
  model {
  for ( i in 1:Ntotal ) {
  y[i] ~ dt( mu[x[i]] , tau[x[i]] , nu )
  }
  for ( j in 1:2 ) {
  mu[j] ~ dnorm( muM , muP )
  tau[j] <- 1/pow( sigma[j] , 2 )
  sigma[j] ~ dunif( sigmaLow , sigmaHigh )
  }
  nu <- nuMinusOne+1
  nuMinusOne ~ dexp(1/29)
  }
  " # close quote for modelString
  # Write out modelString to a text file
  writeLines( modelString , con="BESTmodel.txt" )
  
  #------------------------------------------------------------------------------
  # THE DATA.
  # Load the data:
  y = c( y1 , y2 ) # combine data into one vector
  x = c( rep(1,length(y1)) , rep(2,length(y2)) ) # create group membership code
  Ntotal = length(y)
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    y = y ,
    x = x ,
    Ntotal = Ntotal ,
    muM = mean(y) ,
    muP = 0.000001 * 1/sd(y)^2 ,
    sigmaLow = sd(y) / 1000 ,
    sigmaHigh = sd(y) * 1000 
  )
  
  #------------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Initial values of MCMC chains based on data:
  mu = c( mean(y1) , mean(y2) )
  sigma = c( sd(y1) , sd(y2) )
  # Regarding initial values in next line: (1) sigma will tend to be too big if 
  # the data have outliers, and (2) nu starts at 5 as a moderate value. These
  # initial values keep the burn-in period moderate.
  initsList = list( mu = mu , sigma = sigma , nuMinusOne = 4 )
  
  #------------------------------------------------------------------------------
  # RUN THE CHAINS
  
  parameters = c( "mu" , "sigma" , "nu" )     # The parameters to be monitored
  adaptSteps = 500               # Number of steps to "tune" the samplers
  burnInSteps = 1000
  nChains = 3 
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )
  # Create, initialize, and adapt the model:
  jagsModel = jags.model( "BESTmodel.txt" , data=dataList , inits=initsList , 
                          n.chains=nChains , n.adapt=adaptSteps )
  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )
  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                              n.iter=nIter , thin=thinSteps )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  
  #------------------------------------------------------------------------------
  # EXAMINE THE RESULTS
  if ( showMCMC ) {
    openGraph(width=7,height=7)
    autocorr.plot( codaSamples[[1]] , ask=FALSE )
    show( gelman.diag( codaSamples ) )
    effectiveChainLength = effectiveSize( codaSamples ) 
    show( effectiveChainLength )
  }
  
  # Convert coda-object codaSamples to matrix object for easier handling.
  # But note that this concatenates the different chains into one long chain.
  # Result is mcmcChain[ stepIdx , paramIdx ]
  mcmcChain = as.matrix( codaSamples )
  return( mcmcChain )
  
} # end function BESTmcmc

#==============================================================================

BESTsummary = function( y1 , y2 , mcmcChain ) {
  
  mcmcSummary = function( paramSampleVec , compVal=NULL ) {
    meanParam = mean( paramSampleVec )
    medianParam = median( paramSampleVec )
    dres = density( paramSampleVec )
    modeParam = dres$x[which.max(dres$y)]
    hdiLim = HDIofMCMC( paramSampleVec )
    if ( !is.null(compVal) ) {
      pcgtCompVal = ( 100 * sum( paramSampleVec > compVal ) 
                      / length( paramSampleVec ) )
    } else {
      pcgtCompVal=NA
    }
    return( c( meanParam , medianParam , modeParam , hdiLim , pcgtCompVal ) )
  }
  # Define matrix for storing summary info:
  summaryInfo = matrix( 0 , nrow=9 , ncol=6 , dimnames=list(
    PARAMETER=c( "mu1" , "mu2" , "muDiff" , "sigma1" , "sigma2" , "sigmaDiff" ,
                 "nu" , "nuLog10" , "effSz" ),
    SUMMARY.INFO=c( "mean" , "median" , "mode" , "HDIlow" , "HDIhigh" ,
                    "pcgtZero" ) 
  ) )
  summaryInfo[ "mu1" , ] = mcmcSummary( mcmcChain[,"mu[1]"] )
  summaryInfo[ "mu2" , ] = mcmcSummary( mcmcChain[,"mu[2]"] )
  summaryInfo[ "muDiff" , ] = mcmcSummary( mcmcChain[,"mu[1]"]
                                           - mcmcChain[,"mu[2]"] , 
                                           compVal=0 )
  summaryInfo[ "sigma1" , ] = mcmcSummary( mcmcChain[,"sigma[1]"] )
  summaryInfo[ "sigma2" , ] = mcmcSummary( mcmcChain[,"sigma[2]"] )
  summaryInfo[ "sigmaDiff" , ] = mcmcSummary( mcmcChain[,"sigma[1]"]
                                              - mcmcChain[,"sigma[2]"] , 
                                              compVal=0 )
  summaryInfo[ "nu" , ] = mcmcSummary( mcmcChain[,"nu"] )
  summaryInfo[ "nuLog10" , ] = mcmcSummary( log10(mcmcChain[,"nu"]) )
  
  N1 = length(y1)
  N2 = length(y2)
  effSzChain = ( ( mcmcChain[,"mu[1]"] - mcmcChain[,"mu[2]"] ) 
                 / sqrt( ( mcmcChain[,"sigma[1]"]^2 + mcmcChain[,"sigma[2]"]^2 ) / 2 ) ) 
  summaryInfo[ "effSz" , ] = mcmcSummary( effSzChain , compVal=0 )
  # Or, use sample-size weighted version:
  # effSz = ( mu1 - mu2 ) / sqrt( ( sigma1^2 *(N1-1) + sigma2^2 *(N2-1) ) 
  #                               / (N1+N2-2) )
  # Be sure also to change plot label in BESTplot function, below.
  return( summaryInfo )
}

#==============================================================================

BESTplot = function( y1 , y2 , mcmcChain , ROPEm=NULL , ROPEsd=NULL , 
                     ROPEeff=NULL , showCurve=FALSE , pairsPlot=FALSE ) {
  # This function plots the posterior distribution (and data).
  # Description of arguments:
  # y1 and y2 are the data vectors.
  # mcmcChain is a list of the type returned by function BTT.
  # ROPEm is a two element vector, such as c(-1,1), specifying the limit
  #   of the ROPE on the difference of means.
  # ROPEsd is a two element vector, such as c(-1,1), specifying the limit
  #   of the ROPE on the difference of standard deviations.
  # ROPEeff is a two element vector, such as c(-1,1), specifying the limit
  #   of the ROPE on the effect size.
  # showCurve is TRUE or FALSE and indicates whether the posterior should
  #   be displayed as a histogram (by default) or by an approximate curve.
  # pairsPlot is TRUE or FALSE and indicates whether scatterplots of pairs
  #   of parameters should be displayed.
  mu1 = mcmcChain[,"mu[1]"]
  mu2 = mcmcChain[,"mu[2]"]
  sigma1 = mcmcChain[,"sigma[1]"]
  sigma2 = mcmcChain[,"sigma[2]"]
  nu = mcmcChain[,"nu"]
  if ( pairsPlot ) {
    # Plot the parameters pairwise, to see correlations:
    openGraph(width=7,height=7)
    nPtToPlot = 1000
    plotIdx = floor(seq(1,length(mu1),by=length(mu1)/nPtToPlot))
    panel.cor = function(x, y, digits=2, prefix="", cex.cor, ...) {
      usr = par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y))
      txt = format(c(r, 0.123456789), digits=digits)[1]
      txt = paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex=1.25 ) # was cex=cex.cor*r
    }
    pairs( cbind( mu1 , mu2 , sigma1 , sigma2 , log10(nu) )[plotIdx,] ,
           labels=c( expression(mu[1]) , expression(mu[2]) , 
                     expression(sigma[1]) , expression(sigma[2]) , 
                     expression(log10(nu)) ) , 
           lower.panel=panel.cor , col="skyblue" )
  }
  
  # Set up window and layout:
  openGraph(width=6.0,height=8.0)
  layout( matrix( c(4,5,7,8,3,1,2,6,9,10) , nrow=5, byrow=FALSE ) )
  par( mar=c(3.5,3.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  
  # Select thinned steps in chain for plotting of posterior predictive curves:
  chainLength = NROW( mcmcChain )
  nCurvesToPlot = 30
  stepIdxVec = seq( 1 , chainLength , floor(chainLength/nCurvesToPlot) )
  xRange = range( c(y1,y2) )
  xLim = c( xRange[1]-0.1*(xRange[2]-xRange[1]) , 
            xRange[2]+0.1*(xRange[2]-xRange[1]) )
  xVec = seq( xLim[1] , xLim[2] , length=200 )
  maxY = max( dt( 0 , df=max(nu[stepIdxVec]) ) /
                min(c(sigma1[stepIdxVec],sigma2[stepIdxVec])) )
  # Plot data y1 and smattering of posterior predictive curves:
  stepIdx = 1
  plot( xVec , dt( (xVec-mu1[stepIdxVec[stepIdx]])/sigma1[stepIdxVec[stepIdx]] , 
                   df=nu[stepIdxVec[stepIdx]] )/sigma1[stepIdxVec[stepIdx]] , 
        ylim=c(0,maxY) , cex.lab=1.75 ,
        type="l" , col="skyblue" , lwd=1 , xlab="y" , ylab="p(y)" , 
        main="Data Group 1 w. Post. Pred." )
  for ( stepIdx in 2:length(stepIdxVec) ) {
    lines(xVec, dt( (xVec-mu1[stepIdxVec[stepIdx]])/sigma1[stepIdxVec[stepIdx]] , 
                    df=nu[stepIdxVec[stepIdx]] )/sigma1[stepIdxVec[stepIdx]] , 
          type="l" , col="skyblue" , lwd=1 )
  }
  histBinWd = median(sigma1)/2
  histCenter = mean(mu1)
  histBreaks = sort( c( seq( histCenter-histBinWd/2 , min(xVec)-histBinWd/2 ,
                             -histBinWd ),
                        seq( histCenter+histBinWd/2 , max(xVec)+histBinWd/2 ,
                             histBinWd ) , xLim ) )
  histInfo = hist( y1 , plot=FALSE , breaks=histBreaks )
  yPlotVec = histInfo$density 
  yPlotVec[ yPlotVec==0.0 ] = NA
  xPlotVec = histInfo$mids
  xPlotVec[ yPlotVec==0.0 ] = NA
  points( xPlotVec , yPlotVec , type="h" , lwd=3 , col="red" )
  text( max(xVec) , maxY , bquote(N[1]==.(length(y1))) , adj=c(1.1,1.1) )
  # Plot data y2 and smattering of posterior predictive curves:
  stepIdx = 1
  plot( xVec , dt( (xVec-mu2[stepIdxVec[stepIdx]])/sigma2[stepIdxVec[stepIdx]] , 
                   df=nu[stepIdxVec[stepIdx]] )/sigma2[stepIdxVec[stepIdx]] , 
        ylim=c(0,maxY) , cex.lab=1.75 , 
        type="l" , col="skyblue" , lwd=1 , xlab="y" , ylab="p(y)" , 
        main="Data Group 2 w. Post. Pred." )
  for ( stepIdx in 2:length(stepIdxVec) ) {
    lines(xVec, dt( (xVec-mu2[stepIdxVec[stepIdx]])/sigma2[stepIdxVec[stepIdx]] , 
                    df=nu[stepIdxVec[stepIdx]] )/sigma2[stepIdxVec[stepIdx]] , 
          type="l" , col="skyblue" , lwd=1 )
  }
  histBinWd = median(sigma2)/2
  histCenter = mean(mu2)
  histBreaks = sort( c( seq( histCenter-histBinWd/2 , min(xVec)-histBinWd/2 ,
                             -histBinWd ),
                        seq( histCenter+histBinWd/2 , max(xVec)+histBinWd/2 ,
                             histBinWd ) , xLim ) )
  histInfo = hist( y2 , plot=FALSE , breaks=histBreaks )
  yPlotVec = histInfo$density 
  yPlotVec[ yPlotVec==0.0 ] = NA
  xPlotVec = histInfo$mids
  xPlotVec[ yPlotVec==0.0 ] = NA
  points( xPlotVec , yPlotVec , type="h" , lwd=3 , col="red" )
  text( max(xVec) , maxY , bquote(N[2]==.(length(y2))) , adj=c(1.1,1.1) )
  
  # Plot posterior distribution of parameter nu:
  histInfo = plotPost( log10(nu) , col="skyblue" , # breaks=30 ,
                       showCurve=showCurve ,
                       xlab=bquote("log10("*nu*")") , cex.lab = 1.75 , showMode=TRUE ,
                       main="Normality" ) #  (<0.7 suggests kurtosis)
  
  # Plot posterior distribution of parameters mu1, mu2, and their difference:
  xlim = range( c( mu1 , mu2 ) )
  histInfo = plotPost( mu1 ,  xlim=xlim , cex.lab = 1.75 ,
                       showCurve=showCurve ,
                       xlab=bquote(mu[1]) , main=paste("Group",1,"Mean") , 
                       col="skyblue" )
  histInfo = plotPost( mu2 ,  xlim=xlim , cex.lab = 1.75 ,
                       showCurve=showCurve ,
                       xlab=bquote(mu[2]) , main=paste("Group",2,"Mean") , 
                       col="skyblue" )
  histInfo = plotPost( mu1-mu2 , compVal=0 ,  showCurve=showCurve ,
                       xlab=bquote(mu[1] - mu[2]) , cex.lab = 1.75 , ROPE=ROPEm ,
                       main="Difference of Means" , col="skyblue" )
  
  # Plot posterior distribution of param's sigma1, sigma2, and their difference:
  xlim=range( c( sigma1 , sigma2 ) )
  histInfo = plotPost( sigma1 ,  xlim=xlim , cex.lab = 1.75 ,
                       showCurve=showCurve ,
                       xlab=bquote(sigma[1]) , main=paste("Group",1,"Std. Dev.") , 
                       col="skyblue" , showMode=TRUE )
  histInfo = plotPost( sigma2 ,  xlim=xlim , cex.lab = 1.75 ,
                       showCurve=showCurve ,
                       xlab=bquote(sigma[2]) , main=paste("Group",2,"Std. Dev.") , 
                       col="skyblue" , showMode=TRUE )
  histInfo = plotPost( sigma1-sigma2 , 
                       compVal=0 ,  showCurve=showCurve ,
                       xlab=bquote(sigma[1] - sigma[2]) , cex.lab = 1.75 , 
                       ROPE=ROPEsd ,
                       main="Difference of Std. Dev.s" , col="skyblue" , showMode=TRUE )
  
  # Plot of estimated effect size. Effect size is d-sub-a from 
  # Macmillan & Creelman, 1991; Simpson & Fitter, 1973; Swets, 1986a, 1986b.
  effectSize = ( mu1 - mu2 ) / sqrt( ( sigma1^2 + sigma2^2 ) / 2 )
  histInfo = plotPost( effectSize , compVal=0 ,  ROPE=ROPEeff ,
                       showCurve=showCurve ,
                       xlab=bquote( (mu[1]-mu[2])
                                    /sqrt((sigma[1]^2 +sigma[2]^2 )/2 ) ),
                       showMode=TRUE , cex.lab=1.0 , main="Effect Size" , col="skyblue" )
  # Or use sample-size weighted version:
  # Hedges 1981; Wetzels, Raaijmakers, Jakab & Wagenmakers 2009.
  # N1 = length(y1)
  # N2 = length(y2)
  # effectSize = ( mu1 - mu2 ) / sqrt( ( sigma1^2 *(N1-1) + sigma2^2 *(N2-1) )
  #                                    / (N1+N2-2) )
  # Be sure also to change BESTsummary function, above.
  # histInfo = plotPost( effectSize , compVal=0 ,  ROPE=ROPEeff ,
  #          showCurve=showCurve ,
  #          xlab=bquote( (mu[1]-mu[2])
  #          /sqrt((sigma[1]^2 *(N[1]-1)+sigma[2]^2 *(N[2]-1))/(N[1]+N[2]-2)) ),
  #          showMode=TRUE , cex.lab=1.0 , main="Effect Size" , col="skyblue" )
  return( BESTsummary( y1 , y2 , mcmcChain ) )
} # end of function BESTplot

x = rnorm(100,30,10)
y = rnorm(150,60,20)
mcmcChain = BESTmcmc(x,y)
BESTsummary(x,y,mcmcChain)
z = c(-28,-24)

BESTplot(x,y,mcmcChain,ROPEm =z)  
