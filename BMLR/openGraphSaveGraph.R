# openGraphSaveGraph.R
# John K. Kruschke, 2013

openGraph = function( width=7 , height=7 , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    X11( width=width , height=height , type="cairo" , ... )
  } else { # Windows OS
    windows( width=width , height=height , ... )
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
