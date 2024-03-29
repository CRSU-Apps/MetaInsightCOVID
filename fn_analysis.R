#####################################################################################################
############################################ Frequentist ############################################
#####################################################################################################


##########################
##### function for formating uploaded data
##########################

entry.df <- function(data, CONBI) {
  newData1 <- as.data.frame(data)
  if (ncol(newData1)==6 | ncol(newData1)==5){
    newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
    newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)  # create counting variable for number of arms within each study.
    data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide") # reshape
    numbertreat=max(newData2$number)
  }
  else {
    data_wide<- newData1
    a<- ifelse(CONBI=='Continuous', 4, 3)
    numbertreat=(ncol(newData1)-2)/a
  }
  if (numbertreat < 6) {  # generate additional columns if less than 6 arms.
    for (k in (numbertreat+1):6) {
      if (CONBI=='Continuous') {
      data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      } else {
      data_wide[c(paste0("T.",k),paste0("R.",k),paste0("N.",k))]<-NA
      }
    }
  }
  return(data_wide)
}



#########################
##### function for transforming data to contrast form
#########################

contrastform.df <- function(data, outcome, CONBI) {
  if (CONBI=='Continuous') {
      d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data,sm=outcome)
  } else {
    d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),event=list(R.1,R.2,R.3,R.4,R.5,R.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),data=data,sm=outcome)
  }
  return(d1)
}




#########################
##### function for attaching treatment labels
#########################

labelmatching.df <- function(d1,ntx,treat_list) {
  d1$treat1 <- factor(d1$treat1,
                      levels = c(1:ntx),
                      labels = as.character(treat_list$Label))
  d1$treat2 <- factor(d1$treat2,
                      levels = c(1:ntx),
                      labels = as.character(treat_list$Label))
  return(d1)
}




##########################
##### function for conducting frequentist analysis
##########################

freq.df <- function(model,outcome,dataf,lstx, ref) {
  net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = dataf, subset=NULL,
                  sm = outcome, level=0.95, level.comb=0.95,
                  comb.random=(model=="random"), comb.fixed = (model=="fixed"),reference.group =ref,
                  all.treatments=NULL, seq=NULL, tau.preset=NULL,
                  tol.multiarm = 0.05, tol.multiarm.se = 0.2, warn=TRUE)
  return(net1) 
}




###********************###
### WRAPPING function: function for Wrapping the frequentist data format and analysis
###********************###

freq_wrap <- function(data, treat_list,model,outcome, CONBI, ref) {
  progress <- shiny::Progress$new()   # Adding progress bars
  on.exit(progress$close())
  progress$set(message="Updating", value=0)
  d0<- contrastform.df(data,outcome, CONBI)    # transform data to contrast form
  data_final<- merge(d0,data,by="StudyID")   # for the grouped forest plots only
  lstx <- treat_list$Label      #obtain treatment labels
  ntx <- length(lstx)     #count treatment numbers
  d1<-labelmatching.df(d0, ntx, treat_list) #matching treatment labels to treatment code
  progress$inc(0.6, detail="Updating")
  net1<-freq.df(model,outcome,d1,lstx,ref) # NMA of all studies
  progress$inc(0.4, detail="Rendering results")
  return (list(net1=net1,lstx=lstx, ntx=ntx, data_final=data_final,d0=d0,d1=d1))
}




#########################
##### function for producing group forest plot
#########################

groupforest.df <- function(d1, ntx, lstx, data_final, outcome) {
  text_label <- character()
  n_stud <- integer()
  for (i in 1:ntx) {
    for (j in 1:ntx) {
      if (nrow(d1[(d1$treat1 == i & d1$treat2 == j),]) > 0) {
        text_label <- c(paste(lstx[i], "vs", lstx[j]), text_label)
        n_stud <- c(n_stud, nrow(d1[(d1$treat1 == i & d1$treat2 == j),]))
      }
    }
  }
  gaps <- integer(length(n_stud))
  n_stud <- rev(n_stud)
  for (k in 1:length(n_stud)) {
    if (k == 1) {
      gaps[k] <- n_stud[k] + 1
    }
    else {
      gaps[k] <- gaps[k-1] + n_stud[k] + 2
    }
  }
  lines <- rev(c(1:(nrow(d1) + 2*length(text_label)-1)))
  lines <- lines[!lines %in% gaps]
  lines <- lines[!lines %in% (gaps+1)]
  
  if (outcome == "OR" | outcome =="RR" ){
    fplot <- metafor::forest(d1$TE, sei=d1$seTE, slab = paste(data_final$Study.y), subset = order(d1$treat1, d1$treat2), ylim = c(1, nrow(d1) + 2*length(text_label) + 2),rows = lines, 
    xlim = c(-6, 10), atransf = exp, at = log(c(0.01, 1, 10, 100)) 
    )    
  } 
  else {
    fplot <- metafor::forest(d1$TE, sei=d1$seTE, slab = paste(data_final$Study.y), subset = order(d1$treat1, d1$treat2), ylim = c(1, nrow(d1) + 2*length(text_label) + 2), rows = lines)
  }
  text(-6, gaps, pos=4, font = 4, text_label)
  title("Individual study results (for all studies) grouped by treatment comparison")
  return(fplot)
}

# } else if (outcome == "RD") {
#   fplot <- NULL


##########################
##### function for drawing forest plot
##########################

forest.df <- function(netresult,model,lstx,ref) {
    fp<- metafor::forest(netresult,reference.group=ref, pooled=model)
return(fp)    
}





#######################
##### function for text underneath forest plot
#######################

tau.df <- function(tau,k,n,model) {
  if (model=="random"){
    y<-paste("Between-study standard deviation:", tau,
          ", Number of studies:", k,
          ", Number of treatments:", n)}
  else {
    y<-paste("Between-study standard deviation set at 0. Number of studies:", k,
             ", Number of treatments:", n)}
  return(y)
}



#######################
##### function for exporting netsplit (netmeta) results
#######################

netsplitresult.df <- function(incona, model) {
  Comparison<- incona$comparison
  No.Studies<- as.integer(incona$k)
  
  if (model=="random"){
    Direct<- incona$direct.random$TE
    Indirect<- incona$indirect.random$TE
    Difference<- incona$compare.random$TE
    Diff_95CI_lower<- incona$compare.random$lower
    Diff_95CI_upper<- incona$compare.random$upper
    NMA<- incona$random$TE
    pValue<- incona$compare.random$p}
  else{
    Direct<- incona$direct.fixed$TE
    Indirect<- incona$indirect.fixed$TE
    Difference<- incona$compare.fixed$TE
    Diff_95CI_lower<- incona$compare.fixed$lower
    Diff_95CI_upper<- incona$compare.fixed$upper
    NMA<- incona$fixed$TE
    pValue<- incona$compare.fixed$p}
  df<- data.frame(Comparison, No.Studies, NMA, Direct, Indirect, Difference, Diff_95CI_lower, Diff_95CI_upper, pValue)
  return(df)
}

  
#######################
##### function for progress bar
#######################

progress.df <- function() {
  withProgress(message = 'loading', value = 0, {
    n <- 10
    for (i in 1:n) {
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste(""))
    }
  })
}



#####################################################################################################
############################################ Bayesian ###############################################
#####################################################################################################

### data transform
dataform.df <- function(newData1, treat_list, CONBI) {
  if (ncol(newData1)==6 | ncol(newData1)==5) {
    long <- newData1
  } else {
    data_wide <-newData1
    a<- ifelse(CONBI=='Continuous', 4, 3)
    numbertreat=(ncol(newData1)-2)/a
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        if (CONBI=='Continuous') {
          data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
        } else {
          data_wide[c(paste0("T.",k),paste0("R.",k),paste0("N.",k))]<-NA
        }
      }
    }
    else {
      data_wide<-newData1
    }
    long_pre <- reshape(data_wide, direction = "long",
                        varying = 3:ncol(data_wide), 
                        times=c(".1", ".2", ".3", ".4", ".5", ".6"), sep=".", idvar= c("StudyID", "Study"))
    long_pre<-subset(long_pre, select=-time)
    long <- long_pre[!is.na(long_pre$T), ]
    }
  long_sort<-long[order(long$StudyID, -long$T), ]
  if (CONBI=='Continuous') {
    long_sort$se<-long_sort$SD/sqrt(long_sort$N)
  }
  lstx <- treat_list$Label
  treat_list2<-data.frame(treat_list)
  ntx <- nrow(treat_list)
  colnames(treat_list2)[1] <- "T"
  long_sort2<-merge(long_sort, treat_list2, by=c("T"))
  long_sort2<-subset(long_sort2, select=-T)
  names(long_sort2)[names(long_sort2) == 'Label'] <- 'T'
  return(long_sort2)
} 


### Bayesian analysis

baye <- function(data,treat_list, model, outcome, CONBI, ref) {
  coviddt<-data
  if (outcome=="SMD" | outcome=="RD") {
  } 
  else {
  progress <- shiny::Progress$new()   # Adding progress bars
  on.exit(progress$close())
  progress$set(message="Updating.This may take up to 10 minutes", value=0)
  treat_list2<-data.frame(treat_list)
  if (CONBI=="Continuous") { 
    armData <- data.frame(study=data$Study,       # Create arm level data set for gemtc
                        treatment=data$T,
                        mean=data$Mean,
                        std.err=data$se)
  }
  else {
    armData <- data.frame(study=data$Study,
                          treatment=data$T,
                          responders=data$R,
                          sampleSize=data$N)
  }
  progress$inc(0.2, detail="Preparing to run simulation models")
  mtcNetwork <- mtc.network(data.ab=armData,description="Network")   # Gemtc network object
  if (outcome == "MD") {
    like <- "normal"
    link <- "identity"
  } 
  else  {
    like <- "binom"
      link <- ifelse (outcome == "OR","logit", "log")
    }
    mtcModel <- mtc.model(network=mtcNetwork,
                          type = "consistency",
                          linearModel=model,
                          likelihood=like,
                          link = link,
                          dic=TRUE)
    progress$inc(0.4, detail="Running simulation models")
    mtcResults <- mtc.run(mtcModel)   # Run gemtc model object for analysis
    progress$inc(0.4, detail="Rendering results")
    mtcRelEffects <- relative.effect(mtcResults,t1=ref)  #Set reference treatment
    #mtcRelEffects <- relative.effect(mtcResults,t1=treat_list2[1,2])  #Set reference treatment
    sumresults<-summary(mtcRelEffects)
    a<- paste(model,"effect",sep=" ")   #Create text for random/fixed effect
    cat(mtcResults$model$code, file="codes.txt", fill=FALSE, labels=NULL, append=FALSE)  # write the code into a file for download
    lstx <- treat_list$Label
    ntx <- nrow(treat_list)
    sumoverall<-summary(mtcResults)
    dic<-as.data.frame(sumoverall$DIC)
    list(mtcResults=mtcResults,lstx=lstx,ntx=ntx,treat_list2=treat_list2,mtcRelEffects=mtcRelEffects,
         sumresults=sumresults, a=a, mtcNetwork=mtcNetwork, dic=dic, model=model, outcome=outcome, coviddt=coviddt)
  }}


### 3a. tau of gemtc

gemtctau <- function(results) {
  sumresults<-results$sumresults
  if (results$a=="random effect") {   #SD and its 2.5% and 97.5%
    ntx <- nrow(sumresults$summaries$statistics)
    sd_mean<- round(sumresults$summaries$statistics[ntx,1], digits = 2)
    sd_lowCI<-round(sumresults$summaries$quantiles[ntx,1], digits = 2)
    sd_highCI<-round(sumresults$summaries$quantiles[ntx,5], digits=2)
  }
  else {
    sd_mean =0
    sd_lowCI=0
    sd_highCI=0
  }
  if (results$a=="random effect") {
    paste("Between-study standard deviation:", sd_mean, ". 95% credible interval:",sd_lowCI,", ", sd_highCI, ".")}
  else{paste("Between-study standard deviation set at 0")}
}

### 3d. nodesplit models

bayenode <- function(data, treat_list, model, outcome, CONBI) {
  if (outcome=="SMD" ) {
    print("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
  } 
  else if (outcome=="RD") {
    print("Please note: risk difference currently cannot be analysed in Bayesian analysis", type = "warning")
  }
  else {
    progress <- shiny::Progress$new()   # Adding progress bars
    on.exit(progress$close())
    progress$set(message="Updating.This may take up to 20 minute", value=0)
    treat_list2<-data.frame(treat_list)
    lstx <- treat_list$Label
    ntx <- nrow(treat_list)
    progress$inc(0.2, detail="Preparing to run simulation models")
    if (CONBI=="Continuous") { 
      armData <- data.frame(study=data$Study,       # Create arm level data set for gemtc
                            treatment=data$T,
                            mean=data$Mean,
                            std.err=data$se)
    }
    else {
      armData <- data.frame(study=data$Study,
                            treatment=data$T,
                            responders=data$R,
                            sampleSize=data$N)
    }
    mtcNetwork <- mtc.network(data.ab=armData,description="Network")
    progress$inc(0.4, detail="Running simulation models")
    if (outcome == "MD") {
      like <- "normal"
      link <- "identity"
    } 
    else  {
      like <- "binom"
      link <- ifelse (outcome == "OR","logit", "log")
    }
    nodeSplitResults <- mtc.nodesplit(network=mtcNetwork,
                                      linearModel=model,
                                      likelihood=like,
                                      link = link,
                                      comparisons=mtc.nodesplit.comparisons(mtcNetwork))  # nodesplitting
    progress$inc(0.4, detail="Rendering results")
    node<-as.data.frame(print(summary(nodeSplitResults)))
    node
  }}


### 3f. UME deviance scatter plot

umeplot.df <- function(c,mtcNetwork, model,outcome) {
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message="Updating.", value=0)
  c$names <- rownames(c)
  if (outcome == "MD") {
    like <- "normal"
    link <- "identity"
  } 
  else  {
    like <- "binom"
    link <- ifelse (outcome == "OR","logit", "log")
  }
  ume <- mtc.model(network=mtcNetwork,
                    type = "ume",
                    linearModel= model, 
                    likelihood=like,
                    link = link,
                    dic = TRUE)
  progress$inc(0.5, detail="Preparing results")
  ume_results <- mtc.run(ume)
  progress$inc(0.3, detail="Rendering results")
  y<-mtc.deviance({ume_results})
  inc <-data.frame(y$dev.ab)
  inc$names <- rownames(inc)
  all <-merge(c,inc, by="names")
  names(all)[names(all) == "X1.x"] <- "NMAmodel_arm1"
  names(all)[names(all) == "X1.y"] <- "UMEmodel_arm1"
  names(all)[names(all) == "X2.x"] <- "NMAmodel_arm2"
  names(all)[names(all) == "X2.y"] <- "UMEmodel_arm2"
  k<-all[ , names(all) != "names"]  #### to define the maximum range of the equality line: find the maximum number of the dev data in the dataset.
  j <- max(k, na.rm=TRUE)
  m <- c(0,1,j)
  n <- c(0,1,j)
  dline<-data.frame(m,n)

  p = plot_ly() %>%   # plot
    add_trace(data=dline, x = ~m, y = ~n, type = 'scatter', mode = 'lines',
              line = list(color = '#45171D'))
  p = p %>%
    add_trace(data=all, x=~NMAmodel_arm1, y=~UMEmodel_arm1, type='scatter', mode='markers',
              marker=list(size=4, color = '#CAEFD1',
                          line = list(color = 'rgb(0,128,0)',
                                      width = 2)),
              hoverinfo='text',
              text=~paste('</br> Author', all$names,
                          '</br> Arm 1',
                          '</br> Deviance from NMA model:',round(NMAmodel_arm1, digits=2),
                          '</br> Deviance from UME model:',round(UMEmodel_arm1, digits=2)
              ))
  p=p %>% 
    add_trace(
      x=~NMAmodel_arm2, y=~UMEmodel_arm2, type='scatter', mode='markers', 
      marker=list(size=4, color = '#CAEFD1',
                  line = list(color = 'rgb(0,128,0)',
                              width = 2)),
      hoverinfo='text',
      text=~paste('</br> Author', all$names,
                  '</br> Arm 2',
                  '</br> Deviance from NMA model:',round(NMAmodel_arm2, digits=2),
                  '</br> Deviance from UME model:',round(UMEmodel_arm2, digits=2)
                  
      ))%>%
    layout(showlegend = FALSE, xaxis=list(title="Deviance from NMA model"), 
           yaxis=list(title="Deviance from UME inconsistency model"))
  
  if (ncol(c)>3) { 
    p=p %>% 
      add_trace(data=all,
                x=~X3.x, y=~X3.y, type='scatter', mode='markers', 
                marker=list(size=4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
                hoverinfo='text',
                text=~paste('</br>', all$names,
                            '</br> Arm 3',
                            '</br> Deviance from NMA model:',round(X3.x, digits=2),
                            '</br> Deviance from UME model:',round(X3.y, digits=2)))}
  if (ncol(c)>4) { 
    p=p %>% 
      add_trace(data=all,
                x=~X4.x, y=~X4.y, type='scatter', mode='markers', 
                marker=list(size=4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
                hoverinfo='text',
                text=~paste('</br>', all$names,
                            '</br> Arm 4',
                            '</br> Deviance from NMA model:',round(X4.x, digits=2),
                            '</br> Deviance from UME model:',round(X4.y, digits=2)))}
  if (ncol(c)>5) { 
    p=p %>% 
      add_trace(data=all,
                x=~X5.x, y=~X5.y, type='scatter', mode='markers', 
                marker=list(size=4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
                hoverinfo='text',
                text=~paste('</br>', all$names,
                            '</br> Arm 5',
                            '</br> Deviance from NMA model:',round(X5.x, digits=2),
                            '</br> Deviance from UME model:',round(X5.y, digits=2)))}
  if (ncol(c)>6) { 
    p=p %>% 
      add_trace(data=all,
                x=~X6.x, y=~X6.y, type='scatter', mode='markers', 
                marker=list(size=4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
                hoverinfo='text',
                text=~paste('</br>', all$names,
                            '</br> Arm 6',
                            '</br> Deviance from NMA model:',round(X6.x, digits=2),
                            '</br> Deviance from UME model:',round(X6.y, digits=2)))}
  progress$inc(0.2, detail="Exporting results")
  list(p=p,y=y)
}


### Per-arm residual deviance 

stemplot.df <- function(c,x) {
  tpl <- x[['dev.ab']]
  study <- matrix(rep(1:nrow(tpl), times=ncol(tpl)), nrow=nrow(tpl), ncol=ncol(tpl))
  study <- t(study)[t(!is.na(tpl))]
  devbar <- t(x[['dev.ab']])[t(!is.na(tpl))]
  title <- "Per-arm residual deviance"
  xlab <- "Arm"
  k<-rowSums(!is.na(tpl))
  studynames <- rep(c$names, k)
  v<-1:length(devbar)
  sep<-study%%2
  d<- data.frame(v, devbar, sep, study, studynames)
  xl<- list(
    title = xlab,
    range= c(0,length(devbar)+5),
    tick0 = 0,
    dtick = 5,
    zeroline = TRUE,
    showline = TRUE
  )
  yl <- list(
    title="Residual deviance",
    range=c(0, ceiling(devbar)),
    autorange=TRUE,
    tick0 = 0,
    dtick = 0.5,
    zeroline = TRUE,
    showline = TRUE
  )
  p <- plot_ly(data=d, x=~v, y=~devbar)
  for (i in 1:length(devbar)) {
    p = p %>%
      add_segments(x=i, xend=i, y=0, yend=devbar[i], marker=list(color='white', 
                                                                 line=list(color='white')),
                   line=list(color='black', width=1))
  }
  p = p%>% 
    add_trace(data=d, x=~v, y=~devbar, type = 'scatter', mode='markers', 
              marker=list(size=4, color = '#CAEFD1',
                          line = list(color = 'rgb(0,128,0)',
                                      width = 2)),
              symbol =~sep, symbols = c('circle','o'), 
              hoverinfo='text',
              text=~paste('</br> Study', d$studynames,
                          '</br> Deviance from NMA model:',round(d$devbar, digits=2)
              )) %>%
    layout(
      xaxis = xl, yaxis = yl, showlegend = FALSE)
  return(p)
}



### leverage

levplot.df <- function(x) {
  fit.ab <- apply(x[['fit.ab']], 1, sum, na.rm=TRUE)
  dev.ab <- apply(x[['dev.ab']], 1, sum, na.rm=TRUE)
  lev.ab <- dev.ab - fit.ab
  fit.re <- x[['fit.re']]
  dev.re <- x[['dev.re']]
  lev.re <- dev.re - fit.re
  nd <- c(x[['nd.ab']], x[['nd.re']])
  w <- sqrt(c(dev.ab, dev.re) / nd)
  lev <- c(lev.ab, lev.re) / nd
  d<-data.frame(w,lev)
  d$names <- rownames(d)
  
  a<-seq(from=0, to=3, by=0.05)
  b1 <- 1-a^2
  b2 <- 2-a^2
  b3 <- 3-a^2
  b4<- 4-a^2
  parabola <- data.frame(a, b1, b2, b3, b4)
  
  xlab="Square root of average residual deviance across the arms for each study"
  'sqrt(average(residual deviance for arm 1, residual deviance for arm 2...))'
  ylab="Average leverage across the arms for each study"
  
  xl<- list(
    title = xlab,
    range= c(0,max(c(w,2.5))),
    tick0 = 0,
    dtick = 0.5,
    zeroline = TRUE,
    showgrid = TRUE
  )
  yl <- list(
    title=ylab,
    range=c(0, max(c(lev, 4))),
    tick0 = 0,
    dtick = 1,
    zeroline = TRUE,
    showgrid = TRUE
  )
  
  p<- plot_ly(parabola, x=~a) %>%
    add_trace(y=b1, mode='lines', line=list(color='black', width=1), hoverinfo='skip') %>%
    add_trace(y=b2, mode='lines', line=list(color='black', width=1), hoverinfo='skip') %>%
    add_trace(y=b3, mode='lines', line=list(color='black', width=1), hoverinfo='skip') %>%
    add_trace(y=b4, mode='lines', line=list(color='black', width=1), hoverinfo='skip') %>%
    add_trace(data=d, x=~w, y=~lev, 
              marker=list(size=4, color = '#CAEFD1',
                          line = list(color = 'rgb(0,128,0)',
                                      width = 2)),
              hoverinfo='text',
              text=~paste('</br> Study:', d$names,
                          '</br> Deviance',round(d$w, digits=2),
                          '</br> Leverage',round(d$lev, digits=2)
              )) %>%
    layout(
      xaxis = xl, yaxis = yl, showlegend = FALSE, title="Leverage versus residual deviance")
  return(p)
}





