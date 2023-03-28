utils::globalVariables("SCON")

#' Open spectrometer connection 
#'
#' Open serial connection to the spectrometer.
#'
#' A serial device is identified, and a connection is made to it. 
#'
#' @author Peter Hoff
#' @export
#' @import serial
specOpen<-function(){ 
  sdev<-list.files(path="/dev/serial/by-id")
  port<-Sys.readlink(paste("/dev/serial/by-id/",sdev,sep="")) 
  port<-gsub("../","",port) 
  assign("SCON",serial::serialConnection(port=port,mode="115200,n,8,1"),.GlobalEnv) 
  open(SCON) 
}

#' Confirm command
#'
#' Obtain response from an AT command. 
#'
#' An AT command that doesn't return data should return either 'OK' or 'ERROR'.
#' This command waits for one of these two responses. 
#' 
#' @param timeout time to wait for a response in seconds 
#' @author Peter Hoff
#' @export
confirmCom<-function(timeout=1){ 
  t0<-Sys.time() 
  x<-serial::read.serialConnection(SCON) 
  t1<-Sys.time() 
  while( (t1-t0<timeout) & !is.element(x,c("OK","ERROR")) ){
    x<-serial::read.serialConnection(SCON) 
    t1<-Sys.time() 
  }
  if(t1-t0>=timeout){ x<-"TIMEOUT" }  
  x 
}

#' Get spectrum 
#' 
#' Get spectral data from the device. 
#' 
#' The device returns 18-channel light intensity readings 
#' corresponding to wavelengths ranging from 
#' ultraviolet to infrared. Up to three different 
#' LEDs can be used to illuminate the sample. 
#'
#' @param leds which LEDs to use for illumination
#' @return an 18-dimensional vector of light intensity readings
#' @author Peter Hoff  
#' @export
getSpec<-function(leds=c("UV","VI","IR")){

  if(!exists("SCON") || !serial::isOpen(SCON)){ specOpen() } 

  ## turn off all LEDs
  for(l in 0:5){ 
    serial::write.serialConnection(SCON,paste0("ATLED",l,"=0\n")) 
    confirmCom()
  }
 
  ## identify requested LEDs
  lidx<-c(5,1,3)[ is.element(c("UV","VI","IR"),leds) ] 

  ## turn on requested LEDs 
  for(l in lidx){ 
    serial::write.serialConnection(SCON,paste0("ATLED",l,"=1\n")) 
    confirmCom()
  }  

  ## wait to make sure proper LEDs are on or off
  Sys.sleep(.25)

  ## read data
  serial::write.serialConnection(SCON,"ATCDATA\n") 
  y<-"" ; while(y==""){ y<-serial::read.serialConnection(SCON) }  
  y<-as.numeric(strsplit(substr(y,1,nchar(y)-3),",")[[1]]) 
  y<-y[channelInfo$channelIndex] 
  names(y)<-channelInfo$wavelength

  ## turn off requested LEDS 
  for(l in lidx){ 
    serial::write.serialConnection(SCON,paste0("ATLED",l,"=0\n"))  
    confirmCom() 
  } 

  ## turn indicator light back on 
  serial::write.serialConnection(SCON,"ATLED0=1\n") 

y 

}

#' Get spectral matrix
#' 
#' Get spectrum under multiple illuminations. 
#' 
#' Eighteen spectral readings are obtained separately under each of the three 
#' illumination bulbs (ultraviolet, visible light, infraread), 
#' and the results are returned as a 3 by 18 matrix. 
#'
#' @param plot (logical) plot the spectrum
#' @author Peter Hoff
#' @export
#' @import graphics
getSpecMat<-function(plot=FALSE){ 

  if(!exists("SCON") || !serial::isOpen(SCON)){ specOpen() } 

  Y<-NULL
  for(led in c("UV","VI","IR")){ Y<-rbind(Y,getSpec(leds=led)) }
  rownames(Y)<-c("UV","VI","IR") 
  colnames(Y)<-channelInfo$wavelength  
  if(plot){ 
    par(mfrow=c(3,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0)) 
    for(j in 1:nrow(Y)){ barplot(Y[j,],col=channelInfo$color) } 
  } 

Y
}

#' Channel information
#' 
#' Spectral response information on the 18 channels of the AS7265x. The 
#' variables are as follows:
#' \itemize{ 
#' \item channel name of channel 
#' \item channelIndex order (index) of channel as returned by the device
#' \item wavelength primary wavelength in nm
#' \item color hex value of color 
#' \item red red value for RGB approximation 
#' \item green green value for RGB approximation 
#' \item blue blue value for RGB approximation 
#' }
#' 
#' @docType data
#' @name channelInfo
#' @usage data(channelInfo)
#' @format a data frame with 18 rows and 7 variables 
NULL

#' Get spectral matrices
#' 
#' Get spectral matricies for multiple samples.  
#' 
#' Interactively collects SMs for a series of samples, 
#' allowing for naming of each sample. 
#'
#' @param n (integer) number of replicate scans per sample
#' @author Peter Hoff
#' @export
#' @import abind
getSpecMats<-function(n=5){

  if(!exists("SCON") || !serial::isOpen(SCON)){ specOpen() } 

  p1<-3 ; p2<-18 
 
  SM<-array(dim=c(p1,p2,0))
  snames<-sname<-NULL 

  while( (sname<-readline("Enter sample name or 'stop': ")) !="stop" ){
    X<-matrix(0,p1,p2) 
    for(i in 1:n){ 
      X<-X+getSpecMat()/n  
      Sys.sleep(.25) 
    }
    SM<-abind::abind(SM,X,along=3)  
    snames<-c(snames,sname) 
    } 
  dimnames(SM)[[1]]<-c("UV","VI","IR") 
  dimnames(SM)[[2]]<-channelInfo$wavelength 
  dimnames(SM)[[3]]<-snames 
  SM
}


