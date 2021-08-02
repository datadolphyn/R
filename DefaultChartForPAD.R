# Generate Default Charts for PADS using heuristics
# Author: Jitender Aswani, Co-Founder @datadolph.in
# Date: 3/15/2013
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.


###
# Rules based - time series data - just two columns - 
#
#
###

#
# Get default info for pad
#
getDefault <- function(){
  return(list(dim="", mea="", size="", yaxis=list(), xaxis=list(), chart=list())
}

#
# set default mea
#
setDefaultMea <- function(default,default.mea=""){
  default$mea <- default.mea
  return(default)
}

#
# set default dim
#
setDefaultDim <- function(default,default.dim=""){
  default$dim <- default.dim
  return(default)
}

#
# set pad type
#
setPadType <- function(default, pad.type="timeseries"){
  default$type <- pad.type
  return(default)
}

#
# set yaxis title
#
setYaxisTitle <- function(default, yaxis.title=""){
  default$yaxis$title <- yaxis.title
  return(default)
}

#
# set xaxis title
#
setXaxisTitle <- function(default, xaxis.title=""){
  default$xaxis$title <- xaxis.title
  return(default)
}

#
# set xaxis categories
#
setXaxisCat <- function(default, xaxis.categories=c("")){
  default$xaxis$categories <- as.character(xaxis.categories)
  return(default)
}

#
# set default size
#
setSizeTitle <- function(default, size.title){
  default$size <- as.character(size.title)
  return(default)  
}


#
# setChartType
#
setChartType <- function(default, chart.type="line"){
  default$chart$type=chart.type
  return(default)
}

#
# setChartType Stacked for bar and column
#
setChartTypeStacked <- function(default){
  default$chart$stacked=1
  return(default)
}

#
# setChartTitle
#
setChartTitle <- function(default, chart.title="line"){
  default$chart$title=chart.title
  return(default)
}

#
# add series
#
addSeries <- function(default, series.name, series.data) {
  default$chart$series[[length(default$chart$series)+1]] <- list(name=series.name, data=series.data)
  return(default)
}

#
# get histograms for a pad containing single column
#
getChartForSingleColPad <- function(pad, pmd) {
  #one colummn - prepare a histogram 
  if(verbose) print("PAD has only one column, getting a histogram prepared")
  if(log.all) logMessage("PAD has only one column, getting a histogram prepared")
  
  # Initialize
  histo <- NULL
  default <- getDefault()
  
  #is the column numeric (i.e. mlist exists)
  if((length(pmd$mList) > 0) && (length(pmd$dList)==0)) {
    #h <- hist(pads[1], breaks=8, plot=F)           
    histo <- as.data.frame(table(cut(pad[,1], breaks=8)))
    
    #change col names
    colnames(histo) <- c("groups", "count")
    
    #set default mea
    default <- setDefaultMea(default, names(pmd$mList)[1])
  } else if((length(pmd$dList) > 0) && (length(pmd$mList)==0)) {
    
    #is the column character (i.e. mlist exists)
    histo <- as.data.frame(table(pad[,1]), stringsAsFactors=F)[1:15,]
    
    #change col names
    colnames(histo) <- c(names(pmd$dList)[1], "count")      
    
    #sort based on frequency
    histo <- histo[with(histo, order(-count)), ]
    
    #set default dim
    default <- setDefaultDim(default, names(pmd$dList)[1])
  }
  
  #set pad type
  default <- setPadType(default, "single column")
    
  #set yaxis title
  default <- setYaxisTitle(default, "Count (Frequency)")

  #set xaxis tile and categories
  default <- setXaxisTitle(default, names(histo)[1])
  default <- setXaxisCat(default, histo[,1])
  #print(default)
  #set chart type
  default <- setChartType(default, "column")
  default <- setChartTitle(default,paste("Frequency Plot of", names(pad)[1], sep=" "))
  
  #add a series
  default.series.data <- as.matrix(histo)
  dimnames(default.series.data) <- NULL
  default <- addSeries(default,"Count (Frequency)", default.series.data)
  #print(default)
  
  return(default);
}

#
# get chart for a pad containing two columns
#
getChartForTwoColPad <- function(pad, pmd) {
  # 2 column in this pad
  if(verbose) print("This PAD has two columns, getting a line-chart or column chart or a pie-chart prepared")
  if(log.all) logMessage("This PAD has two columns, getting a line-chart or column chart or a pie-chart prepared")
  
  # Initialize
  default <- getDefault()
  pad.dt <- data.table(pad)
  
  # if both columns are measures, a scatter plot
  if((length(pmd$mList) == 2 ) && (length(pmd$dList)==0)) {
    if(verbose) print("Both columns are numeric , this is not a time-series pad")
    if(log.all) logMessage("Both columns are numeric , this is not a time-series pad") 
    pad.type <- "other"

    ## mea - the first numeric column as the measure
    default.mea <- names(pmd$mList[which(pmd$mList == "numeric")])[1]
    default <- setDefaultMea(default, default.mea)
    
    #set yaxis title
    default <- setYaxisTitle(default, default.mea)
    
    # default measure data
    default.mea.data <- as.vector(as.character(pad.dt[,get(default.mea)]))
    
    ## dim - the second numeric column as the dim
    default.dim <- names(pmd$mList[which(pmd$mList == "numeric")])[2]
    
    #set default dim
    default <- setDefaultDim(default, default.dim)
    
    #set xaxis tile and categories
    default <- setXaxisTitle(default, default.dim)   
    default.dim.data <- as.vector(as.character(pad.dt[,get(default.dim)])) 
    default <- setXaxisCat(default, default.dim.data)
    
    #set chart type & chart title
    default <- setChartType(default, "column")
    default <- setChartTitle(default,pmd$title)
    
    #set pad type
    default <- setPadType(default, pad.type )
    
    #add a series
    default.series.data=as.matrix(cbind(default.dim.data, default.mea.data))
    dimnames(default.series.data) <- NULL
    default <- addSeries(default,default.mea, default.series.data)
    
    #print(default)
    
    #return
    return(default)
    
  } else if((length(pmd$mList) == 0 ) && (length(pmd$dList)==2)) {
    # if both columns are dims - don't know what to do
    if(verbose) print("Both columns are non-numeric , this is not a time-series pad")
    if(log.all) logMessage("Both columns are non-numeric , this is not a time-series pad")  
    #
    #
    #
    # To be implemented
    # 
    #
    #
    #
    return(default)
  } else if((length(pmd$mList) > 0) && (length(pmd$dList) > 0)) {
    
    if(verbose) print("There is at least one dim and one measure, this is a ok pad")
    if(log.all) logMessage("There is at lease one dim and one measure, this is a ok pad")
    
    ## mea - the first numeric column as the measure
    default.mea <- names(pmd$mList[which(pmd$mList == "numeric")])[1]
    
    if(!is.null(default.mea) && !is.na(default.mea)) { 
      
      default <- setDefaultMea(default, default.mea)
      #set yaxis title
      default <- setYaxisTitle(default, default.mea)
      
      # default measure data
      default.mea.data = as.vector(as.character(pad.dt[,get(default.mea)]))                                              
      
      #
      # Get the first dim of type date
      #
      default.dim <- names(pmd$dList[which(pmd$dList == "Date")])[1]
      
      if(is.null(default.dim) || is.na(default.dim)) { 
        # date is not a dimension in this pad
        if(verbose) print("This is not a time-series pad")
        if(log.all) logMessage("This is not a time-series pad")
        
        # Setting the pad type    
        pad.type <- "other"
        
        #
        # dim of type date doesn't exist, finding factor
        #
        if(verbose) print("dim of type date doesn't exist, finding factor")
        if(log.all) logMessage("dim of type date doesn't exist, finding factor")
        
        tem <- names(pmd$dList[which(pmd$dList == "character")])
        
        if(!is.null(tem) && !is.na(tem) && (length(tem) > 0)) { 
          #check which dim can be of type factors 
          
          tem2 = tem[which(length(as.factor(pad[,tem])) != length(pad[,tem]))]
          if(!is.null(tem2) && !is.na(tem2) && (length(tem2) > 0)) {
            #if dims can be coverted to factor, use the first dim as default
            default.dim = tem2[1] 
            if(verbose) print("Found a dim which is a factor")
            if(log.all) logMessage("Found a dim which is a factor")
            
            default.dim.data = as.character(pad.dt[,get(default.dim)])
            default.series.data=as.character(pad.dt[,sum(get(default.mea), na.rm=TRUE), 
                                                    by=get(default.dim)])
            default.chart.type="column"              
            default.xaxis.categories <- levels(as.factor(pad.dt[,get(default.dim)]))                
          } else { 
            #same as scatter plot when using date as default dim
            if(verbose) print("Found a dim which is a character") 
            if(log.all) logMessage("Found a dim which is a character")
            default.dim = tem[1]
            #print(pad.dt)
            default.dim.data = as.character(pad.dt[,get(default.dim)])
            #print(default.dim.data)
            default.series.data <- cbind(as.character(pad.dt[,get(default.dim)]), as.character(pad.dt[,get(default.mea)]) )
            #print(default.series.data)
            
            dimnames(default.series.data) <- NULL
            
            default.chart.type <- "bar"            
            default.xaxis.categories = as.character(pad.dt[,get(default.dim)])              
          }
          
          #set default dim
          default <- setDefaultDim(default, default.dim)
          
          #set xaxis tile and categories
          default <- setXaxisTitle(default, default.dim)   
          default <- setXaxisCat(default, default.xaxis.categories)
          
          #set chart type & chart title
          default <- setChartType(default, default.chart.type)
          default <- setChartTitle(default, pmd$title)
          
          #set pad type
          default <- setPadType(default, pad.type)
          
          #add a series
          dimnames(default.series.data) <- NULL
          default <- addSeries(default,default.mea, default.series.data)
          
          #print(default)
          
          #return
          return(default)
                 
        } else { 
          if(verbose) print("dim is null: no dimensions found, bad pad") 
          if(log.all) logMessage("dim is null: no dimensions found, bad pad")           
          #pmd <- c(pmd,type=pad.type)
        }
      } else { # if dim of date exists
        if(verbose) print("Found a dim which is a date") 
        if(log.all) logMessage("Found a dim which is a date")
        
        pad.type <- "timeseries"
        default.dim.data = as.character(pad.dt[,get(default.dim)])
        
        #set default dim
        default <- setDefaultDim(default, default.dim)
        
        #set xaxis tile and categories
        default <- setXaxisTitle(default, default.dim)   
        default <- setXaxisCat(default, default.dim.data)
        
        #set chart type & chart title
        default <- setChartType(default, "line")
        default <- setChartTitle(default, pmd$title)
        
        #set pad type
        default <- setPadType(default, pad.type )
        
        #add a series
        default.series.data <- as.matrix(cbind(as.character(pad.dt[,get(default.dim)]), as.character(pad.dt[,get(default.mea)]) ))
        dimnames(default.series.data) <- NULL
        default <- addSeries(default,default.mea, default.series.data)
        #print(default)
        
        #return
        return(default)
      }
    }
  }
}

#
# get chart for a pad containing three columns
#
getChartForThreeColPad <- function(pad, pmd) {
  if(verbose) print("This PAD has three columns, sending it to chartForThreeColPad  to get a line-chart or column chart or a pie-chart prepared")
  if(log.all) logMessage("This PAD has more than three columns, sending it to chartForThreeColPad  to get a line-chart or column chart or a pie-chart prepared")
  
  # 3 column in this pad
  # Get the first mea of type numeric
  
  # Initialize
  default <- getDefault()
  pad.dt <- data.table(pad)
  #assign("pad.dt", pad.dt, envir=.GlobalEnv)
  #assign("pmd", pmd, envir=.GlobalEnv)
  
  # if two columns are measures and the other one is a dim
  if((length(pmd$mList) == 2 ) && (length(pmd$dList)==1)) { 
    ## need to plot two line charts
    if(verbose) print("Two measures and one dimension in this pad")
    if(log.all) logMessage("Two measures and one dimension in this pad") 
  
    pad.type <- "other"  
    
    ## dim - the only column as the dim - check if it is date
    default.dim <- names(pmd$dList[which(pmd$dList == "Date")])[1]
    if(!is.null(default.dim) && !is.na(default.dim)) {
      pad.type <- "timeseries"
      default.chart.type="line"
    } else { #assuming character vecotr
      default.dim = names(pmd$dList[which(pmd$dList == "character")])[1] 
      pad.type <- "other"
      default.chart.type="column"
    }
    
    ## mea - the first numeric column as the measure
    default.mea <- names(pmd$mList[which(pmd$mList == "numeric")])[1]
    default <- setDefaultMea(default, default.mea)
        
    # default measure data
    default.mea.data = as.vector(as.character(pad.dt[,get(default.mea)]))
    
    ## mea - the second numeric column as the measure
    default.mea.second = names(pmd$mList[which(pmd$mList == "numeric")])[2]
    default.mea.second.data = as.vector(as.character(pad.dt[,get(default.mea.second)]))
    default.yaxis.second.title=default.mea.second
   
    #set yaxis title
    default <- setYaxisTitle(default, paste(default.mea, default.mea.second, sep="-"))
    
    #set chart type & chart title
    default <- setChartType(default, default.chart.type)
    default <- setChartTitle(default,pmd$title)
    
    #set default dim
    default <- setDefaultDim(default, default.dim)
    
    #set xaxis tile and categories
    default <- setXaxisTitle(default, default.dim)   
    default.dim.data <- as.vector(as.character(pad.dt[,get(default.dim)])) 
    default <- setXaxisCat(default, default.dim.data)
    
    #set pad type
    default <- setPadType(default, pad.type )
    
    #add a series
    default.series.data=as.matrix(cbind(default.dim.data, default.mea.data))
    dimnames(default.series.data) <- NULL
    default <- addSeries(default,default.mea, default.series.data)
    #print(default)
    ## add a second sereis
    default.series.second.data=as.matrix(cbind(default.dim.data, default.mea.second.data))
    dimnames(default.series.second.data) <- NULL
    default <- addSeries(default,default.mea.second, default.series.second.data)
    #print(default)
    
    #return
    return(default)   
  } else if((length(pmd$mList) == 3 ) && (length(pmd$dList)==0)) {
    #
    # All three columns are numeric, this is not a time-series pad, preparing for bubble chart

    #
    if(verbose) print("All three columns are numeric, this is not a time-series pad, preparing for bubble chart")
    if(log.all) logMessage("All three columns are numeric, this is not a time-series pad, preparing for bubble chart")  
    
    ## mea - the first numeric column as the measure
    default.mea = names(pmd$mList[which(pmd$mList == "numeric")])[1]
    default <- setDefaultMea(default, default.mea)
    
    #set yaxis title
    default <- setYaxisTitle(default, default.mea)
    
    # default measure data
    default.mea.data = as.character(pad.dt[,get(default.mea)])
    
    ## dim - the second numeric column as the dim
    default.dim = names(pmd$mList[which(pmd$mList == "numeric")])[2]
    
    #set default dim
    default <- setDefaultDim(default, default.dim)
    
    #set xaxis tile and categories
    default <- setXaxisTitle(default, default.dim)   
    
    default.dim.data = as.character(pad.dt[,get(default.dim)])
    default <- setXaxisCat(default, default.dim.data)
    
    ## size - the third numeric column as the size
    default.size = names(pmd$mList[which(pmd$mList == "numeric")])[3]
    default.size.data = as.character(pad.dt[,get(default.size)])
    default <- setSizeTitle(default, default.size)
    
    #set chart type & chart title
    default <- setChartType(default, "bubble")
    default <- setChartTitle(default,pmd$title)
    
    #prepare a series
    default.series.data=as.matrix(cbind(default.dim.data, default.mea.data, default.size.data))
    dimnames(default.series.data) <- NULL
    
    #add a series
    default <- addSeries(default, default.size, default.series.data)
    
    #return
    return(default)   
    
  }  else if((length(pmd$mList) == 1 ) && (length(pmd$dList)==2)) {
    # if(length(levels(as.factor(pad.dt[,get(default.dim)]))) != length(pad.dt[,get(default.dim)]))

    if(verbose) print("Two columns are non-numeric i.e. dimensions ")
    if(log.all) logMessage("Two columns are non-numeric i.e. dimensions")  
   
    pad.type <- "other"  

    ## dim - the only column as the dim - check if it is date
    default.dim <- names(pmd$dList[which(pmd$dList == "Date")])[1]
    if(!is.null(default.dim) && !is.na(default.dim)) {
      pad.type <- "timeseries"
      default.chart.type="line"
    } else { #assuming character vecotr
      default.dim = names(pmd$dList[which(pmd$dList == "character")])[1] 
      pad.type <- "other"
      default.chart.type="bar"
    }
    
    #set pad type
    default <- setPadType(default, pad.type )
    #set chart type & chart title
    default <- setChartType(default, default.chart.type)
    default <- setChartTitle(default,pmd$title)
        
    ## mea - the first numeric column as the measure
    default.mea <- names(pmd$mList[which(pmd$mList == "numeric")])[1]
    default <- setDefaultMea(default, default.mea)
    #set yaxis title
    default <- setYaxisTitle(default, default.mea)
    # default measure data
    default.mea.data = as.vector(as.character(pad.dt[,get(default.mea)]))
    
    ## dim - take the second dim as the x-axis
    ## The best thing to do will be a to find which of the two dim columns are factor

    #set xaxis tile and categories
    default <- setXaxisTitle(default, default.dim)   
    default.dim.data <- as.vector(as.character(pad.dt[,get(default.dim)])) 
    default.dim.factor <- F
    #check for factor
    if(length(levels(as.factor(pad.dt[,get(default.dim)]))) != length(pad.dt[,get(default.dim)])){
      if(verbose) print(paste(default.dim, " is a factor", sep=""))
      default.dim.factor <- T
      default.dim.data <- as.vector(levels(as.factor(pad.dt[,get(default.dim)])))
    }
    #lapply(tem, function(x) length(levels(as.factor(pad.dt[,get(x)]))) != length(pad.dt[,get(x)]))
    
    #check whether second dim is also a factor
    default.dim.second = names(pmd$dList[which(pmd$dList == "character")])[2]
    default.dim.second.data <- as.vector(as.character(pad.dt[,get(default.dim.second)])) 
    default.dim.second.factor <- F
    if(length(levels(as.factor(pad.dt[,get(default.dim.second)]))) != length(pad.dt[,get(default.dim.second)])){
      #print(paste(default.dim.second, " is a factor", sep=""))
      default.dim.second.factor <- T
      default.dim.second.data <- as.vector(levels(as.factor(pad.dt[,get(default.dim.second)])))
    }
    
    if(default.dim.factor && default.dim.second.factor) {
      #add a series
      setkeyv(pad.dt, default.dim.second)
      for(i in default.dim.second.data){
        series.name = i
        #print(series.name)
        data = as.vector(as.character(pad.dt[series.name][, get(default.mea)]))
        series.data=as.matrix(cbind(default.dim.data, data))
        dimnames(series.data) <- NULL
        default <- addSeries(default,series.name, series.data)
      }      
      default <- setDefaultDim(default, default.dim)
      default <- setXaxisCat(default, default.dim.data)    
      default <- setChartTypeStacked(default)
    } else if (default.dim.factor && !default.dim.second.factor) {
      default.series.data=as.matrix(cbind(default.dim.second.data, default.mea.data))
      dimnames(default.series.data) <- NULL
      default <- addSeries(default,default.mea, default.series.data)
      default <- setDefaultDim(default, default.dim.second)
      default <- setXaxisCat(default, default.dim.second.data)    
      
      # use first dim as special labels
    } else if (!default.dim.factor && default.dim.second.factor) {
      default.series.data=as.matrix(cbind(default.dim.data, default.mea.data))
      dimnames(default.series.data) <- NULL
      default <- addSeries(default,default.mea, default.series.data)
      default <- setDefaultDim(default, default.dim)
      # use first dim as special labels
    }
    #print(default)

    return(default)
    
  } else if((length(pmd$mList) == 0 ) && (length(pmd$dList)==3)) {
    # if both columns are dims - don't know what to do
    if(verbose) print("All three columns are dimensions, don't know what to do")
    if(log.all) logMessage("All three columns are dimensions, don't know what to do")   
  }
}

#
# get chart for a pad containing Four columns
#
getChartForFourColPad <- function(pad, pmd) {
  if(verbose) print("This PAD has four columns...")
  if(log.all) logMessage("This PAD has four columns...")
  
  # 4 column in this pad
  # Get the first mea of type numeric
  
  # Initialize
  default <- getDefault()
  pad.dt <- data.table(pad)
  # if three columns are measures and the other one is a dim
  if((length(pmd$mList) == 3 ) && (length(pmd$dList)==1)) { 
    if(verbose) print("Three measures and one dimension in this pad")
    if(log.all) logMessage("Three measures and one dimension in this pad") 
    
    pad.type <- "other"  
    ## mea - the first numeric column as the measure
    default.mea <- names(pmd$mList[which(pmd$mList == "numeric")])[1]
    default <- setDefaultMea(default, default.mea)
    
    #set yaxis title
    default <- setYaxisTitle(default, default.mea)
    
    # default measure data
    default.mea.data = as.vector(as.character(pad.dt[,get(default.mea)]))
    
    ## mea - the second numeric column as the measure
    default.mea.second = names(pmd$mList[which(pmd$mList == "numeric")])[2]
    default.mea.second.data = as.vector(as.character(pad.dt[,get(default.mea.second)]))
    default.yaxis.second.title=default.mea.second

    ## mea - the third numeric column as the measure
    default.mea.3rd = names(pmd$mList[which(pmd$mList == "numeric")])[3]
    default.mea.3rd.data = as.vector(as.character(pad.dt[,get(default.mea.3rd)]))
    default.yaxis.3rd.title=default.mea.3rd
    
    ## dim - the only column as the dim - check if it is date
    default.dim <- names(pmd$dList[which(pmd$dList == "Date")])[1]
    if(!is.null(default.dim) && !is.na(default.dim)) {
      pad.type <- "timeseries"
      default.chart.type="line"
    } else { #assuming character vecotr
      default.dim = names(pmd$dList[which(pmd$dList == "character")])[1] 
      pad.type <- "other"
      default.chart.type="bar"
    }
    
    #set chart type & chart title
    default <- setChartType(default, default.chart.type)
    default <- setChartTitle(default,pmd$title)
    
    #set default dim
    default <- setDefaultDim(default, default.dim)
    
    #set xaxis tile and categories
    default <- setXaxisTitle(default, default.dim)   
    default.dim.data <- as.vector(as.character(pad.dt[,get(default.dim)])) 
    default <- setXaxisCat(default, default.dim.data)
    
    #set pad type
    default <- setPadType(default, pad.type )
    
    #add a series
    default.series.data=as.matrix(cbind(default.dim.data, default.mea.data))
    dimnames(default.series.data) <- NULL
    default <- addSeries(default,default.mea, default.series.data)
    #print(default)
    
    ## add a second sereis
    default.series.second.data=as.matrix(cbind(default.dim.data, default.mea.second.data))
    dimnames(default.series.second.data) <- NULL
    default <- addSeries(default,default.mea.second, default.series.second.data)

    ## add a third sereis
    default.series.3rd.data=as.matrix(cbind(default.dim.data, default.mea.3rd.data))
    dimnames(default.series.3rd.data) <- NULL
    default <- addSeries(default,default.mea.3rd, default.series.3rd.data)
    
    #return
    return(default)   
  }
}


getChartForMultiColPad <- function(pad, pmd) {
  if(verbose) print("This PAD has many columns...")
  if(log.all) logMessage("This PAD has many columns...")

  # Initialize
  default <- getDefault()
  pad.dt <- data.table(pad)
  # if three columns are measures and the other one is a dim
  if((length(pmd$dList)==1)) { 
    if(verbose) print("One dimension in this pad, assuming rest are measures")
    if(log.all) logMessage("One dimension in this pad, assuming rest are measures") 
        
    ## dim - the only column as the dim - check if it is date
    default.dim <- names(pmd$dList[which(pmd$dList == "Date")])[1]
    if(!is.null(default.dim) && !is.na(default.dim)) {
      pad.type <- "timeseries"
      default.chart.type="line"
    } else { #assuming character vecotr
      default.dim = names(pmd$dList[which(pmd$dList == "character")])[1] 
      pad.type <- "other"
      default.chart.type="bar"
    }
    
    #set chart type & chart title
    default <- setChartType(default, default.chart.type)
    default <- setChartTitle(default,pmd$title)
    
    #set default dim
    default <- setDefaultDim(default, default.dim)
    
    #set xaxis tile and categories
    default <- setXaxisTitle(default, default.dim)   
    default.dim.data <- as.vector(as.character(pad.dt[,get(default.dim)])) 
    default <- setXaxisCat(default, default.dim.data)
    
    #set pad type
    default <- setPadType(default, pad.type )
    
    ## mea - the first numeric column as the measure
    default.mea <- names(pmd$mList[which(pmd$mList == "numeric")])[1]
    default <- setDefaultMea(default, default.mea)
    
    #set yaxis title
    default <- setYaxisTitle(default, default.mea)
       
    #add a series
    for(i in 1:length(pmd$mList)){
      series.name = names(pmd$mList[which(pmd$mList == "numeric")])[i]
      data = as.vector(as.character(pad.dt[,get(series.name)]))
      series.data=as.matrix(cbind(default.dim.data, data))
      dimnames(series.data) <- NULL
      default <- addSeries(default,series.name, series.data)
    }

    #return
    return(default)   
  }
}



#
# prepare default chart for a pad
#
getDefaultChartInfo <- function(pad, pmd) {#pmd is pad meta data
  if(verbose) print("Starting the default chart identification process")
  
  #converting the pad data.frame to a data.table
  pad.dt <- data.table(pad)
  
  #check the column count in the pad
  # Continue if there is atleast one column 
  
  l <- NULL
  if(pmd$columns > 4 ) {
    if(verbose) print("This PAD has more than four columns, sending it to chartForTwoColPad  to get a line-chart or column chart or a pie-chart prepared")
    if(log.all) logMessage("This PAD has more than four columns, sending it to chartForTwoColPad  to get a line-chart or column chart or a pie-chart prepared")
    
    l <- getChartForMultiColPad(pad, pmd)
  } else {
    l <- switch(pmd$columns, 
           getChartForSingleColPad(pad, pmd), 
           getChartForTwoColPad(pad, pmd),  
           getChartForThreeColPad(pad, pmd),
           getChartForFourColPad(pad, pmd)
    )
  }
  #print(l)
  if(!is.null(l)) 
    pmd <- c(pmd, type=l$type, default=list(l))
  return(pmd)    
}
