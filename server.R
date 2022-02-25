######  COVID ######

#if packages not installed, please install them first by running the line below.
#install.packages(c("dplyr","metafor", "netmeta","shiny", "shinyAce","rmarkdown", "knitr", "gemtc", "plyr", data.table"
#  , "shinyalert", "plotly"))

# the data for meta-regression is from: http://nicedsu.org.uk/wp-content/uploads/2016/03/TSD3-Heterogeneity.final-report.08.05.12.pdf

library(dplyr)
library(netmeta)
library(metafor)
library(shiny) 
library(shinyAce)
library(rmarkdown)
library(knitr)
library(gemtc)
library(plyr)
library(data.table)
library(shinyalert)
library(plotly)
library(shinyjs)
library(BUGSnet)
library(googlesheets4)




source("PlotFunctionsRKO.R", local = TRUE)        # Plot functions
load("blank.rds")                                 # Objects to store data for plot functions
source("fn_analysis.R",local = TRUE)              # functions for NMA
source("additional_fn_covid.R",local = TRUE) 
source("net.R",local = TRUE) 
source("autoconnect_function.R", local = TRUE)    # function for automatican network connection
source("APIKey_Private.R", local = TRUE)          # adds in our personal API key
  
shinyServer(function(input, output, session) {
  source("downloadbuttons_continuous.R", local = TRUE)   #codes for download buttons for conciseness. This line must be put within the shinyserver as this is purely a code file not functions.
  #options(shiny.sanitize.errors = FALSE)
  
gs4_auth_configure(api_key = API_key) # sets up API key for googlesheets 
gs4_deauth() # setting for google sheets to not need authentication


# URL for data
url_og <- "https://docs.google.com/spreadsheets/d/1yutijO8Kp-4xYJ1xyoybObS-srZABUgwctHBL3-rQVM/edit?usp=sharing" #data on google sheets, where anyone with this link can view/edit

url <- reactive({
  if (input$caption=="") {
    url <- url_og
  } else {
    url <- input$caption
  }
}) 
# have it either as our data, or data the user puts in (google sheet url)
  
# instructions for data formatting
output$covid_instruction <- downloadHandler(
  filename <- function() {
    paste("MetaInsight_COVID_dataset_instructions","pdf", sep = ".")
  },
  content <- function(file){
    file.copy("./MetaInsight_COVID_dataset_instructions.pdf", file)
  }
)


  ############################################
  ############# COVID 19 PAGE ################
  ############################################
  
  ### 0  date. # no longer updating
  timecheck <- function(){
    #dayweek <- format(Sys.time(), "%A")
    #if (dayweek== "Saturday"){
    #  day<-5
    #} else if (dayweek== "Sunday") {
    #  day<-6
    #} else if (dayweek== "Monday") {
    #  day <-0
    #} else if (dayweek== "Tuesday") {
    #  day <-1
    #}else if (dayweek== "Wednesday") {
    #  day <-2
    #}else if (dayweek== "Thursday") {
    #  day <-3
    #}else {
    #  day<-4
    #}
    #updatetime<-strptime("2020-07-01 16:00:00", "%Y-%m-%d %H:%M:%S")   # each time when publishing the new update, put in the date and time here the date and time it published.
    #y<- as.numeric(difftime(Sys.time(), updatetime , units="days"))
    #if (y>1.5) {
    #  displaytime <- format(Sys.time()-86400*(day),"%d %B %Y")         
    #} else{
    # displaytime <- format(updatetime,"%d %B %Y")
    #}
     updatetime<-strptime("2020-10-19 8:00:00", "%Y-%m-%d %H:%M:%S")   # each time when publishing the new update, put in the date and time here the date and time it published.
     displaytime <- format(updatetime,"%d %B %Y")
    displaytime
  }
  
  output$time <- renderText({
    timecheck()
  })
  
  output$time2 <- renderText({
    timecheck()
  })
  

  
  
  ### 1. Load COVID data
  
  
  # COVID all data
  
  # Read in and store data once when radio button chosen (if an option has been chosen for a second time, it returns the stored data)
  CachedData <- reactiveValues()
  observe(CachedData$mortality_data <- read_sheet(url(), sheet = "COVIDMortalitydata", col_names = FALSE))
  observe(CachedData$mortality_label <- read_sheet(url(), sheet = "COVIDMortalitylabel"))
  observe(CachedData$conversion_data <- data.frame())
  observe(CachedData$conversion_label <- data.frame())
  observe(CachedData$SAE_data <- data.frame())
  observe(CachedData$SAE_label <- data.frame()) #initialise default datasets
  
  
  COVIDchosen <- reactive({
    if (input$COVIDoutc=='mortality') {
      dt <- CachedData$mortality_data
      dt <- firstrowhead(dt)
      lb <- CachedData$mortality_label
    } else if (input$COVIDoutc=='conversion') {
      if (is.empty(CachedData$conversion_data)==TRUE) { # if 'conversion' option has not yet been selected
        CachedData$conversion_data <- read_sheet(url(), sheet = "COVIDviralconversiondata", col_names = FALSE)
        CachedData$conversion_label <- read_sheet(url(), sheet = "COVIDviralconversionlabel")
      }
      dt <- CachedData$conversion_data
      dt <- firstrowhead(dt)
      lb <- CachedData$conversion_label
    } else {
      if (is.empty(CachedData$SAE_data)==TRUE) {
        CachedData$SAE_data <- read_sheet(url(), sheet = "COVIDSAEdata",col_names = FALSE)
        CachedData$SAE_label <- read_sheet(url(), sheet = "COVIDSAElabel")
      }
      dt <- CachedData$SAE_data
      dt <- firstrowhead(dt)
      lb <- CachedData$SAE_label
    }
    lb <- lb[order(lb$Number),] #ensure labels are ordered
    data <- dt  # keep one without labelling.
    ntx <- nrow(lb)
    dt$T <- factor(dt$T,
                   levels = c(1:ntx),
                   labels = as.character(lb$Label))
    dt
    list(dt=dt, data=data, lb=lb)
  })
  
  defaultD <- reactive({
    data<-COVIDchosen()$data
    data<-data[,1:5]
    data
  })
  

  # COVID connected data
  
  data_connected <- reactive({
    dt<-COVIDchosen()$data # taking exported data
    lb<-COVIDchosen()$lb #taking exported labels
    connected<-auto_connect(data=dt, treatment_labels=lb, studyid="StudyID", treatment="T", trtcode="Number", auto_drop = TRUE, auto_recode = TRUE, study_pos=1, trt_pos=3, trtcode_pos=1) # take connected network of dataset
    dt<-connected$data # new connected dataset
    lb<-connected$labels #new connected treatment labels
    dt<-dt[,1:5]
    list(dt=dt, lb=lb)
  })
  
  data <- reactive({
    data_connected()$dt
  })
  
  
  
  

  ### 2. data table at the front page
  
  output$COVIDtb <- DT::renderDataTable(DT::datatable({
    if(is.null(data())){return()}
    COVIDchosen()$dt
  }, options = list(processing=FALSE, editable=TRUE), rownames= FALSE, 
  colnames=c('StudyID', 'Author','Treatment','Number of people who had outcome in each arm','Sample size in each arm',
               'Follow up days', 'Dose', 'Treatment duration', 'Risk of bias', 'Patient characteristics', 'Country', 'Time of outcome measure (days)', 'Included*'),
  filter = list(position = 'top', clear = FALSE, stateSave = TRUE)
    ))

  

  ### 3. Filtering function so that the network and forest plot can analyze in realtime with the filtered pool of studies.
  
  # option 1: use filtering
  
  filterdt <- reactive({
    data<-COVIDchosen()$data[input[["COVIDtb_rows_all"]], ]
    data<-data[,1:5]
    data
  })
  
  #  dt_connect_original<- reactive({
  #   if (!is.null(input$COVIDtb_rows_selected) | identical(filterdt(),defaultD())!= TRUE ) {
  #     
  #   }
  #   
  # })
  # 
  # filterdt_char <- reactive({
  #   data<-COVIDchosen()$data[input[["COVIDtb_rows_all"]], ]
  #   data
  # })
  
  excludedstudy<-reactive({
    newData <- data()
    newData1 <- as.data.frame(newData)
    if (ncol(newData1)==6 ||ncol(newData1)==5 ){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
      newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
      data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
    }
    else {
      data_wide<- newData1
    }
    filtd<-filterdt()
    filtd1 <- as.data.frame(filtd)
    if (ncol(filtd1)==6 ||ncol(filtd1)==5 ){
      filtd2<-filtd1[order(filtd1$StudyID, -filtd1$T), ]
      filtd2$number<- ave(as.numeric(filtd2$StudyID),filtd2$StudyID,FUN=seq_along)
      filtd3 <- reshape(filtd2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
    }
    else {
      filtd3<- filtd1
    }

    if (identical(filterdt(),defaultD())==TRUE) {
      x<- ""
    } else {
      y<- as.character(filtd3$Study)
      z <- as.character(data_wide$Study)
      x<-z[!(z %in% y)]
    }
      excl <-x
    return(excl)
  })
  
  
  # option 2: selected rows.
  
  v <- reactiveValues()
  v$s <- NULL
  
  observe({
    if(!is.null(input$COVIDtb_rows_selected)){
      v$s <- input$COVIDtb_rows_selected
    }
  })
      
    
  seldata <- reactive({
    selected <- COVIDchosen()$data[v$s,1:5]
  })
  

  
  output$test_selectmore <- renderText({         # reminder test to ask users to select more than one studies to be included.
    if(!is.null(input$COVIDtb_rows_selected)){
      if ((length(unique(seldata()$StudyID)))<2)
       print("Please select more than one studies to be included.")
  }}
  )
  
  output$none <- renderText({         # reminder test to inform users no studies are selected.
    if(!is.null(input$COVIDtb_rows_selected) | identical(filterdt(),defaultD())!= TRUE ){
      print("Please make sure all arms of a study are selected before proceeding to the network plot and forest plot results.")
    } else {
      print("All studies that marked as 'Yes' for inclusion in the datatable above are currently included in the analysis. 
            Try clicking each individual study in the table to create your own combination, or using the filter box under each column heading to 
            include a subgroup of studies, e.g. select 'moderate / severe' for 'patient characteristics'. ")
    }
    
    }
  )
  
  
  ### 3+1 data table to confirm the included studies.
  
  output$seldt <- DT::renderDataTable({
    coln <- c('StudyID', 'Author','Treatment','Number of people who had outcome in each arm','Sample size in each arm',
              'Follow up days', 'Dose', 'Treatment duration', 'Risk of bias', 'Patient characteristics', 'Country', 'Time of outcome measure (days)', 'Included')  
    if(!is.null(input$COVIDtb_rows_selected)){
      dt <- COVIDchosen()$dt[v$s,]
      dt$Included <- gsub("Not yet", "Included upon individual selection", dt$Included)
      dt <-dt[order(dt$StudyID),]
      dt
    } else if (identical(filterdt(),defaultD())!=TRUE) {
      dt <- COVIDchosen()$dt[input[["COVIDtb_rows_all"]], ]
      dt$Included <- gsub("Not yet", "Not included", dt$Included)       # because they are using our existing filters. so do not have the highest level of freedom.
      dt
    }
    if (('Included upon individual selection' %in% dt$Included) | ('Not included' %in% dt$Included)) {
    coln <- c('StudyID', 'Author','Treatment','Number of people who had outcome in each arm','Sample size in each arm',
              'Follow up days', 'Dose', 'Treatment duration', 'Risk of bias', 'Patient characteristics', 'Country', 'Time of outcome measure (days)', 'Included*')  # add * for notes.
    }
    DT::datatable(dt,
                  options = list(processing=FALSE, editable=FALSE), rownames= FALSE, selection="none",
                  #colnames=c('StudyID', 'Author','Treatment','Number of people who had outcome in each arm','Sample size in each arm',
                   #          'Follow up days', 'Dose', 'Treatment duration', 'Risk of bias', 'Patient characteristics', 'Country', 'Outcome (days)', 'Included*')
                  colnames = coln
                   )})
  
  output$included_upon_sel <- renderText({
    if(!is.null(input$COVIDtb_rows_selected)){
      dt <- COVIDchosen()$dt[v$s,]
      if ('Not yet' %in% dt$Included) {
        print("* The studies that are labeled as 'not yet' in the overall table are selected manually into the analysis. They are now labeled as 'Included upon individual selection' and included in the analysis providing the network is connected. 
              Please check the network connectivity before proceeding to running the model. ")
      }
    } else if (identical(filterdt(),defaultD())!=TRUE) {
      dt <- COVIDchosen()$dt[input[["COVIDtb_rows_all"]], ]
      if ('Not yet' %in% dt$Included) {
        print("* The studies that are labeled as 'not yet' in the overall table due to causing network to be disconnected are labeled as 'not included' and will not be included in the analysis. If you would like to include them, please use the individual study selection function, whereby you can select each study manually by clicking the row where they are located.")
    }}
    })
  
  
  #### 3+2 Refernce treatment if treatment 1 is removed from the network
  
  ref_alter_covid <- function(){
    ref_all <- "Standard_care"
    ref_second <- "Placebo"
    if (((ref_all %in% dt_confirm()$long_sort2_sub$T) ) == "TRUE") {
      ref_sub<- ref_all
    } else if (((ref_second %in% dt_confirm()$long_sort2_sub$T) ) == "TRUE") {
      ref_sub <- ref_second
    } else {
      ref_sub <- as.character(dt_confirm()$long_sort2_sub$T[1])
    }
    return(ref_sub)
  }
  
  
  output$ref_change_note <- renderText({
    #if (identical(ref_alter_covid()$ref_sub, "Standard_care")=="FALSE") {
      paste("Please note that the reference treatment for sensitivity analysis has now been changed to:", ref_alter_covid(), ". This is because the treatment labelled 1 has been removed from the network of sensitivity analysis." )
    #}
  })
  

  ### 4. network plot at the COVID page
  
  freq_sub_covid= function(netdata,lb, ref){
    data_wide <-  entry.df(netdata,"Binary")
    data_sub <- filter(data_wide, !Study %in% excludedstudy())  # Get subset of data to use
    treat_list <- lb
    outc <- "OR"
    freq_wrap(data_sub, treat_list,"random",outc, "Binary", ref)
  }
  
  bugsnetdt <- reactive({
    newData1 <- as.data.frame(data())
    treat_list <- data_connected()$lb
    longsort2 <- dataform.df(newData1,treat_list,"Binary")    # inputting the data in long form
    return(longsort2)
  })

   network_COVID <- function(){
   if (input$networkstyle_sub_COVID=='networkp1') {    #next step
     if (!is.null(input$COVIDtb_rows_selected)== FALSE) {
       connected<-auto_connect(data=data(), treatment_labels=data_connected()$lb, studyid="StudyID", treatment="T", trtcode="Number", auto_drop = TRUE, auto_recode = TRUE, study_pos=1, trt_pos=3, trtcode_pos=1, zero_arms=TRUE) #ensure network is connected after zero arms removed (as is done with this network plot)
       lb<-connected$labels
       data<-connected$data
      } 
      else {
        lb <- COVIDchosen()$lb
        data<-seldata()
      }
     make_netgraph(freq_sub_covid(data,lb, ref_alter_covid()))

    } else {
      if (length(input$COVIDtb_rows_selected)== 0) {
      long_sort2_sub <- filter(bugsnetdt(), !Study %in% excludedstudy())  # subgroup
      data.rh<-data.prep(arm.data=long_sort2_sub, varname.t = "T", varname.s="Study")
      net.plot(data.rh, node.scale = 3, edge.scale=1.5)      # THIS LINE PRODUCES: Warning: Error in : Column `id` must be length 0 (the number of rows) or one, not 2
      } else {
        newData1 <- as.data.frame(seldata())
        treat_list <- COVIDchosen()$lb
        longsort2 <- dataform.df(newData1,treat_list,"Binary")
        data.rh<-data.prep(arm.data=longsort2, varname.t = "T", varname.s="Study")
        net.plot(data.rh, node.scale = 2, edge.scale=2 , study.counts = TRUE,edge.lab.cex=1)   
      }
      
      }
   }
 
  output$netGraphUpdating_COVID <- renderPlot({
    long_sort2_sub<-dt_confirm()$long_sort2_sub
    freqcount<-table(long_sort2_sub$StudyID)
    if (min(freqcount)>1) {
    network_COVID()
    }
  }) 
  
  output$texttry <- renderTable({
    filterdt()

  })
  
  
  
  ### 4+1 notification before the disconnected network plot
  
  output$overalldiscon <- renderText({
    if (!is.null(input$COVIDtb_rows_selected) | identical(filterdt(),defaultD())!= TRUE ) {
      print("Please note: the network plot of subselected studies (after applying filtering or individual selection by hand) is displayed.") 
    } else {
      print("Please note: there exist two separate networks. The primary network is displayed below.
               If you would like to view the overall disconnected network, please click the link below.")
    }
  })
  
  output$seltbshow <- renderUI({
    if (!is.null(input$COVIDtb_rows_selected) | identical(filterdt(),defaultD())!= TRUE ) {
      p("Please view the included studies by clicking", actionLink("viewseldt", tags$strong("
            here")),tags$strong("."))
    }
  })
  
  
  observeEvent(input$viewseldt, ({
    updateCollapse(session, "selec", open = "View the included studies after filtering or individual selection (Click to open / hide this panel)")
  }))
  
  
  
  ### 5. Disconnected overall network plot
  
  network_all_dis <- function() {
      newData1 <- as.data.frame(defaultD())
      treat_list <- COVIDchosen()$lb
      longsort2 <- dataform.df(newData1,treat_list,"Binary")    # inputting the data in long form
      data.rh<-data.prep(arm.data=longsort2, varname.t = "T", varname.s="Study")
      net.plot(data.rh, node.scale = 2, edge.scale=2 , study.counts = TRUE,edge.lab.cex=1)      
    title("Network plot of disconnected overall work")

  }
  output$networkall_dis <- renderPlot({
    network_all_dis()
  })
  
  

  
  ### 5+1 Confirmed data
  
  dt_confirm <- reactive({
    if (length(input$COVIDtb_rows_selected)== 0) {
      newData1 <- as.data.frame(data())
      treat_list <- data_connected()$lb
      longsort2 <- dataform.df(newData1,treat_list,"Binary")
      long_sort2_sub <- filter(longsort2, !Study %in% excludedstudy())  # subgroup
    } else {
      dt<-as.data.frame(seldata())
      treat_list <- COVIDchosen()$lb
      long_sort2_sub <- dataform.df(dt,treat_list,"Binary")
    }
    return(list(long_sort2_sub=long_sort2_sub, treat_list=treat_list))
  })
  
  


  
  
  
  ### 6. Forest plot

  
  output$freq_sub_covid<- renderPlot({
    if (!is.null(input$COVIDtb_rows_selected)== FALSE) {
      connected<-auto_connect(data=data(), treatment_labels=data_connected()$lb, studyid="StudyID", treatment="T", trtcode="Number", auto_drop = TRUE, auto_recode = TRUE, study_pos=1, trt_pos=3, trtcode_pos=1, zero_arms=TRUE) #ensure network is connected after zero arms removed (as is done with frequentist analysis)
      lb<-connected$labels
      data<-connected$data
    }
    else {
      data<-seldata()
      lb<-COVIDchosen()$lb
    }
    freq_list <-freq_sub_covid(data,lb, ref_alter_covid())
        if (input$COVIDoutc != "conversion") {
          left.label="Intervention is better"
          right.label="Reference is better"
        } else {
          left.label="Reference is better"
          right.label="Intervention is better"
        }
    metafor::forest(freq_list$net1,reference.group=ref_alter_covid(),pooled="random", xlim=c(0.1, 10), label.left=left.label, label.right=right.label)
    output$ref4<- renderText({"All outcomes are versus the reference treatment"})
    title("Results")
  })
  
  output$text_sea_disp=output$text_freq_disp <- renderText({
    a<-add_count(dt_confirm()$long_sort2_sub, StudyID)
    a2<-filter(a,n>2)
    a3<-filter(a2,R!=0)
    a4<-add_count(a3, StudyID)
    if ((1 %in% a4$n)==TRUE) {
      paste("Please note that if the plot is not shown, it could be due to the following reasons: a) The frequentist analysis automaticlaly 
                            removes the study arms that contain 0 event in multi-arm trials so for example if Yueping et al is included, it will be left with a single arm, causing the analysis not proceeded; b) The network is disconnected. In either case, please check data and re-select studies.")
    }
  })
  

  
  
  # model_sub_covid <- eventReactive(input$baye_sub_COVID, {
  #   long_sort2_sub<-dt_confirm()$long_sort2_sub
  #   treat_list<-dt_confirm()$treat_list
  #   r<-table(long_sort2_sub$StudyID)
  #   if (min(r)>1) {
  #     baye(long_sort2_sub,treat_list,"random", "OR","Binary", ref_alter_covid())
  #   }
  # })
  # 
  # samedt <- function(){    # These lines are to tell Shiny to change the plot to blank when data are filtered, i.e. analysis needs to be re-done.
  #   long_sort2_sub<-dt_confirm()$long_sort2_sub
  # }
  # 
  # output$gemtc_sub_covid <- renderPlot({
  #   if (identical(model_sub_covid()$coviddt,samedt())==FALSE) {
  #   } else {
  #     if (input$COVIDoutc != "conversion") {
  #       left.label="Intervention is better"
  #       right.label="Standard care is better"
  #     } else {
  #       left.label="Standard care is better"
  #       right.label="Intervention is better"
  #     }
  #     forest(model_sub_covid()$mtcRelEffects,digits=3, xlim=c(log(0.1), log(10)), left.label=left.label, right.label=right.label)
  # 
  #     title(paste("Bayesian", model_sub_covid()$a,"consistency model forest plot results"))
  #   }
  # })
  # 
  
  



  ### 7. Links on this page to connect to other pages.
  
  # Connect to the introduction page
  
  observeEvent(input$infoclick, {
    newvalue <- "Intro"
    updateNavbarPage(session,"meta", selected="Project introduction")
  })
  
  # Connect to the user manual page
  
  observeEvent(input$manualclick, {
    newvalue <- "guide"
    updateNavbarPage(session,"meta", selected="User Guide")
  })
  
  # connect to the analysis page
  
  observeEvent(input$detailedANclick, {
    newvalue <- "analysis"
    updateNavbarPage(session,"meta", selected="Detailed data analysis")
  })
  
    
  ### 8. disconnection and only one study left notification
  
  disconnect_covid <- function(){
    showModal(modalDialog(
      title = "Disconnected network",
      easyClose = FALSE,
      p(tags$strong("Please note that the network is disconnected. Two or more networks exist. The disconnected networks are displayed at the 'Network plot' section. 
                      Please either exclude studies until only one network exists, or add studies to link the networks together.")),
      br(),
      modalButton("Close warning"),
      footer = NULL
    ))
  }
  
  morethanone <- function(){
    showModal(modalDialog(
      title = "Only one study exists",
      easyClose = FALSE,
      p(tags$strong("Please note that there is only one study existing in the network after screening and therefore the network meta analysis cannot be conducted. Some functions may not be executed.")),
      br(),
      modalButton("Close warning"),
      footer = NULL
    ))
  }
  
  # reference_t <- function(){
  #   showModal(modalDialog(
  #     title = "Standard care treatment has been removed",
  #     easyClose = FALSE,
  #     p(tags$strong("Please note that the standard care is currently set as the reference treatment in the model. The model will not be executed if the standard care is removed from the network. 
  #                   We are working on this to allow more flexibility on this setting and will update as soon as possible. Alternatively, please feel free to check out our regular freely available MetaInsight app which allows users to format and upload their own data
  #                   and enjoy the full flexibility of the app.")),
  #     br(),
  #     modalButton("Close warning"),
  #     footer = NULL
  #   ))
  # }
  
  
  onearm <- function(){
    showModal(modalDialog(
      title = "Only one arm included",
      easyClose = FALSE,
      p(tags$strong("Only one arm of a study is included. Please check the datatable and ensure all the arms of a study are included.")),
      br(),
      modalButton("Close warning"),
      footer = NULL
    ))
  }
  
  
  observeEvent(input$baye_sub_COVID,{
    long_sort2_sub<-dt_confirm()$long_sort2_sub
    treat_list<-dt_confirm()$treat_list
    
    
    r<-table(long_sort2_sub$StudyID)
    if (min(r)==1) {
      onearm()
    } else {
    
    
    if ((length(unique(long_sort2_sub$StudyID)))==1) {
      morethanone()
    } else if ((length(unique(long_sort2_sub$StudyID)))>1) {
      sumtb_sub <- bugsnet_sumtb(long_sort2_sub)
      if (sumtb_sub$Value[6]=="FALSE") {
          disconnect_covid()
        } 
    } else {}
    
    # if (("Standard_care" %in% long_sort2_sub$T)==FALSE) {
    #     reference_t()
    #   }
      
    }})
  

    
    

  
  observeEvent(input[["COVIDtb_rows_all"]],{   # can't combine the event of this one together with the one above to simplify the code. When combined, the app will load with a 'disconnected' notification, probably because it detects before the datatable even loaded.
    newData1 <- as.data.frame(data())
    treat_list <- data_connected()$lb
    longsort2 <- dataform.df(newData1,treat_list,"Binary" )
    long_sort2_sub <- filter(longsort2, !Study %in% excludedstudy())  # subgroup
    
    r<-table(long_sort2_sub$StudyID)
    if (min(r)==1) {
      onearm()
    } else {
    
    if ((length(unique(long_sort2_sub$StudyID)))==1) {
      morethanone()
    } else if ((length(unique(long_sort2_sub$StudyID)))>1) {
      sumtb_sub <- bugsnet_sumtb(long_sort2_sub)
      if (sumtb_sub$Value[6]=="FALSE") {
        disconnect_covid()
      }} else {}
      
    }})
  
  
  # Code below not working. so warning notification is not given at the moment when individually selecting study and cause the network to be disconnected. Further think. this is not necessary.becasue it will keep giving messages.
  # observe({
  # if (!is.null(input$COVIDtb_rows_selected)) {
  #   newData1 <- as.data.frame(seldata())
  #   treat_list <- COVIDchosen()$lb
  #   long_sort2_sub <- dataform.df(newData1,treat_list,"Binary")
  #   if ((length(unique(long_sort2_sub$StudyID)))>1) {
  #     sumtb_sub <- bugsnet_sumtb(long_sort2_sub)
  #     if (sumtb_sub$Value[6]=="FALSE") {
  #       disconnect_covid()
  #     }
  #   }}
  # })
  
  


  
  ############################################
  ########### Data analysis tab ##############
  ############################################
  
  ### Confirmation for the outcome
    
    outcometext <- function(){
      if (input$COVIDoutc=="mortality") {
        t <- "All cause mortality"
      } else if (input$COVIDoutc=="conversion") {
          t<- "Incidence of viral negative conversion"
      } else {
          t<-"Incidence of serious adverse events"
      }
      t
    }
    
    output$CONBI2 <- renderText({
    paste("You have selected", "<font color=\"#ffd966\"><b>" , outcometext(),"</b></font>", 
          "outcome on the 'COVID19' page. The analysis for ",
          "<font color=\"#ffd966\"><b>", outcometext(),"</b></font>", "outcome is now displayed.")
    })
    
    
    
    
  ### Get studies for check box input.
    
    # output$Choicesexcl <- renderUI({
    #   newData <- data()
    #   newData1 <- as.data.frame(newData)
    #   if (ncol(newData1)==6 ||ncol(newData1)==5 ){        
    #     newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
    #     newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
    #     data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")    
    #   }
    #   else {
    #     data_wide<- newData1
    #   }
    #   filtd<-filterdt()
    #   filtd1 <- as.data.frame(filtd)
    #   if (ncol(filtd1)==6 ||ncol(filtd1)==5 ){       
    #     filtd2<-filtd1[order(filtd1$StudyID, -filtd1$T), ]
    #     filtd2$number<- ave(as.numeric(filtd2$StudyID),filtd2$StudyID,FUN=seq_along)  
    #     filtd3 <- reshape(filtd2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")     
    #   }
    #   else {
    #     filtd3<- filtd1
    #   }
    #   
    #   if (identical(filterdt(),defaultD())==TRUE) {
    #     x<- ""
    #   } else {
    #     y<- as.character(filtd3$Study)
    #     z <- as.character(data_wide$Study)
    #     x<-z[!(z %in% y)]
    #   }
    #   checkboxGroupInput("exclusionbox",
    #                      label = NULL,
    #                      choices = as.character(data_wide$Study), selected = x)    # checkbox selection is responsive to the filter applied at the 'COVID' page.
    # 
    # })
    
    output$Choicesexcl <- renderUI({
      newData <- data()
      newData1 <- as.data.frame(newData)
      if (ncol(newData1)==6 ||ncol(newData1)==5 ){        
        newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
        newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
        data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")    
      }
      else {
        data_wide<- newData1
      }

      checkboxGroupInput("exclusionbox",
                         label = NULL,
                         choices = as.character(data_wide$Study))    # checkbox selection is responsive to the filter applied at the 'COVID' page.
      
    })
    
    ### Refernce treatment if treatment 1 is removed from the network
    
    ref_alter <- function(){
      newData1 <- as.data.frame(data())
      treat_list <- data_connected()$lb
      lstx <- treat_list$Label
      ref_all <- as.character(lstx[1])
      longsort2 <- dataform.df(newData1,treat_list,"Binary" )
      long_sort2_sub <- filter(longsort2, !Study %in% input$exclusionbox)  # subgroup
      if (((lstx[1] %in% long_sort2_sub$T) ) == "TRUE") {
        ref_sub<- as.character(lstx[1])
      } else {
        ref_sub <- as.character(long_sort2_sub$T[1])
      }
      list(ref_all=ref_all, ref_sub=ref_sub)
    }
    
    
    # output$ref_change_bay = output$ref_change <- renderText({
    #   if (identical(ref_alter()$ref_sub, ref_alter()$ref_all)=="FALSE") {
    #     paste("Please note that the reference treatment for sensitivity analysis has now been changed to:", ref_alter()$ref_sub, ". This is because the treatment labelled 1 has been removed from the network of sensitivity analysis." )
    #   }
    # })
    
    ####
    
    observeEvent(filterdt2(), {
      newData <- data()
      newData1 <- as.data.frame(newData)
      if (ncol(newData1)==6 ||ncol(newData1)==5 ){        
        newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
        newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
        data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")    
      }
      else {
        data_wide<- newData1
      }
      filtd<-filterdt2()
      filtd1 <- as.data.frame(filtd)
      if (ncol(filtd1)==6 ||ncol(filtd1)==5 ){       
        filtd2<-filtd1[order(filtd1$StudyID, -filtd1$T), ]
        filtd2$number<- ave(as.numeric(filtd2$StudyID),filtd2$StudyID,FUN=seq_along)  
        filtd3 <- reshape(filtd2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")     
      }
      else {
        filtd3<- filtd1
      }
      
      if (identical(filterdt2(),defaultD())==TRUE) {
        x<- ""
      } else {
        y<- as.character(filtd3$Study)
        z <- as.character(data_wide$Study)
        x<-z[!(z %in% y)]
      }
      
      
      updateCheckboxGroupInput(session, "exclusionbox",
                               label=NULL,
                               choices = as.character(data_wide$Study), selected = x) 
      
    })

    
    
    output$test <- renderText( {
      excludedstudy()
    })
    
    
    
    ### Get data table
    
    output$datatb <- DT::renderDataTable(DT::datatable({
      if(is.null(data())){return()}
      COVIDchosen()$dt
    },editable=TRUE, rownames= FALSE, 
    colnames=c('StudyID', 'Author','Treatment','Number of people who had outcome in each arm','Sample size in each arm',
               'Follow up days', 'Dose', 'Treatment duration', 'Risk of bias', 'Patient characteristics', 'Country', 'Time of outcome measure (days)', 'Included*'),
    filter = list(
      position = 'top', clear = FALSE, stateSave = TRUE)
    
    ))
    
    filterdt2 <- reactive({
      data<-COVIDchosen()$data[input[["datatb_rows_all"]], ]
      data<-data[,1:5]
      data
    })
    

    ### open data table
    
    observeEvent(input$datatablebutton, ({
      updateCollapse(session, "collapse2", open = "View data (Click to open / hide this panel)")
    }))
    
  
    ### Radiobutton
    # observeEvent(input$COVIDoutc=='conversion', ({
    #   updateRadioButtons(session, "rankopts",'For treatment rankings, smaller outcome values  
    #                   (e.g. smaller mean values for continuous data, or in some cases, 
    #                   e.g. log ORs, more negative values, for binary data) are:', c("Desirable" = "good", "Undesirable" = "bad"), selected = "bad" )
    # }))
    
    
    output$rankbutton <- renderUI({
      if (input$COVIDoutc=='conversion') {
        sel <- "bad"
      } else {
        sel <- "good"
      }
      radioButtons('rankopts', 'For treatment rankings, smaller outcome values
                      (e.g. smaller mean values for continuous data, or in some cases,
                      e.g. log ORs, more negative values, for binary data) are:',
                   c("Desirable" = "good", "Undesirable" = "bad"), selected = sel)
    })
    
    
    

    
  #####################
  #### Frequentist ####
  #####################
    
      
  ### Frequentist analysis
    
  freq_all= function(){
      connected<-auto_connect(data=data(), treatment_labels=data_connected()$lb, studyid="StudyID", treatment="T", trtcode="Number", auto_drop = TRUE, auto_recode = TRUE, study_pos=1, trt_pos=3, trtcode_pos=1, zero_arms=TRUE) #ensures data is connected after removal of zero studies which frequentist does
      data_wide <- entry.df(connected$data,"Binary")    #transform data to wide form
      #label <- ifelse("Binary"=="Continuous",input$listCont,input$listbina)
      treat_list <- connected$labels
      #treat_list <- read.csv(text=label, sep = "\t")   #read treatment labels from input
      outc <- ifelse ("Binary"=="Continuous",input$outcomeCont, input$outcomebina)
      freq_wrap(data_wide, treat_list,input$modelranfix,outc,"Binary", ref_alter()$ref_all)  # use the selfdefined function, freq_wrap

  }
  freq_sub= function(){
    connected<-auto_connect(data=data(), treatment_labels=data_connected()$lb, studyid="StudyID", treatment="T", trtcode="Number", auto_drop = TRUE, auto_recode = TRUE, study_pos=1, trt_pos=3, trtcode_pos=1, zero_arms=TRUE)
    data_wide <- entry.df(connected$data,"Binary") 
    data_sub <- filter(data_wide, !Study %in% input$exclusionbox)  # Get subset of data to use
    #label <- ifelse("Binary"=="Continuous",input$listCont,input$listbina)
    treat_list <- connected$labels
    #treat_list <- read.csv(text=label, sep = "\t")
    outc <- ifelse ("Binary"=="Continuous",input$outcomeCont, input$outcomebina)
    freq_wrap(data_sub, treat_list,input$modelranfix,outc, "Binary", ref_alter()$ref_sub)
  }
  


  
  ### 1b. Study results forest plot
    
  make_netStudy = function() {
    freq=freq_sub()
    outc <- ifelse ("Binary"=="Continuous",input$outcomeCont, input$outcomebina)
    groupforest.df(freq$d0, freq$ntx, freq$lstx, freq$data_final, outc)
  }
  output$forestPlot <- renderPlot({
    make_netStudy()
  })
  

  
  ### 1c. Network Plot
  make_netgraph = function(freq) {  
    netgraph(freq$net1, lwd=2, number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=1.25, cex.points=2, col.points=1, col=8, pos.number.of.studies=0.43)
  }

  
  output$netGraphStatic <- renderPlot({
    if (input$networkstyle=='networkp1') {
      make_netgraph(freq_all())
    } else {
      data.rh<-data.prep(arm.data=bugsnetdt(), varname.t = "T", varname.s="Study")
      net.plot(data.rh, node.scale = 3, edge.scale=1.5) 
    }
    title("Network plot of all studies")
  })
  
  output$netGraphUpdating <- renderPlot({
    if (input$networkstyle_sub=='networkp1') {
      make_netgraph(freq_sub())
    } else {
      long_sort2_sub <- filter(bugsnetdt(), !Study %in% input$exclusionbox)  # subgroup
      data.rh<-data.prep(arm.data=long_sort2_sub, varname.t = "T", varname.s="Study")
      net.plot(data.rh, node.scale = 3, edge.scale=1.5)
    }
    title("Network plot with studies excluded")
  })
  
  make_netconnect = function(freq) {    # network connectivity
    d1 <- freq$d1
    nc1 <- netconnection(d1$treat1,d1$treat2,d1$studlab, data=NULL)
    print(nc1)
  }
  output$netconnect <- renderPrint ({
    make_netconnect(freq_all())
  })
  output$netconnect_sub <- renderPrint ({
    make_netconnect(freq_sub())
  })
  
  

  
  
  
  ############### bugsnet code #################
  
  ### 1a. Data characteristics
  
  bugsnet_sumtb <- function(data){
    data.rh<-data.prep(arm.data=data, varname.t = "T", varname.s="Study")
    if ("Binary"=="Continuous") {
      outcome = "Mean"
      typeO= "continuous"
    } else {
      outcome = "R"
      typeO = "binomial"
    }
    network.char <- net.tab(data = data.rh,
                            outcome = outcome,
                            N = "N",
                            type.outcome = typeO,
                            time = NULL)
    return(network.char$network)
  }
  
  output$sumtb <- renderTable({
    longsort2 <- bugsnetdt()    # inputting the data in long form
    bugsnet_sumtb(longsort2)
  })
  
  output$sumtb_sub <- renderTable({
    longsort2 <- bugsnetdt()    # inputting the data in long form
    longsort2_sub <- filter(bugsnetdt(), !Study %in% input$exclusionbox)  # subgroup
    bugsnet_sumtb(longsort2_sub)
  })
  
  
  ### (notification on disconnection)
  disconnect <- function(){
      showModal(modalDialog(
        title = "Disconnected network",
        easyClose = FALSE,
        p(tags$strong("Please note that the network is disconnected. Two or more networks exist. The disconnected networks are displayed at 'Data analysis' - '1c. Network Plot' tab. 
                      Please either keep excluding studies until only one network exist, or add back studies to link the networks together.")),
        br(),
        modalButton("Close warning"),
        footer = NULL
      ))
  }
  observeEvent(input$data,{
    longsort2 <- bugsnetdt()    # inputting the data in long form
    sumtb<-bugsnet_sumtb(longsort2)
    if (sumtb$Value[6]=="FALSE") {
      disconnect()
    }})
  

  
  observeEvent(input$exclusionbox,{
    newData1 <- as.data.frame(data())
    treat_list <- data_connected()$lb
    longsort2 <- dataform.df(newData1,treat_list,"Binary" )
    long_sort2_sub <- filter(longsort2, !Study %in% input$exclusionbox)  # subgroup
    if ((length(unique(long_sort2_sub$StudyID)))==1) {
      morethanone()
    } else if ((length(unique(long_sort2_sub$StudyID)))>1) {
      sumtb_sub <- bugsnet_sumtb(long_sort2_sub)
      if (sumtb_sub$Value[6]=="FALSE") {
        disconnect()
      }
    } else{}
  })

  
  
  


  ### 2a. Forest Plot
  
  make_netComp = function(freq, ref) {    # forest plot
    forest.df(freq$net1,input$modelranfix,freq$lstx,ref )
    output$ref4<- renderText({"All outcomes are versus the reference treatment (treatment labelled 1)"})
  }
  output$Comparison2<- renderPlot({
    make_netComp(freq_all(), ref_alter()$ref_all)
    title("Results for all studies")
  })
  output$SFPUpdatingComp <- renderPlot({
    make_netComp(freq_sub(), ref_alter()$ref_sub)
    title("Results with studies excluded")
  })
  
  texttau = function(freq){      # Tau
    tau<- round(freq$net1$tau,2)
    tau.df(tau, freq$net1$k, freq$net1$n, input$modelranfix)
  }
  output$textcomp<- renderText({
    texttau(freq_all())
  })
  output$text5<- renderText({ 
    texttau(freq_sub())
  })
  
  
  
  
  ### 2b. Comparison and rank table
  
  make_netrank = function(freq) {
    model <- input$modelranfix
    league <- netleague(freq$net1, comb.random=(model=="random"), comb.fixed = (model=="fixed"), digits =2, seq= netrank(freq$net1, small = input$rankopts))
    if (model=="random"){
      leaguedf<- as.data.frame(league$random)
    }
    else {
      leaguedf<- as.data.frame(league$fixed)
    }
    leaguedf
  }
  output$rankChartStatic<- renderTable(colnames=FALSE,{
    make_netrank(freq_all())
  })
  output$rankChartUpdating<- renderTable(colnames=FALSE,{
    make_netrank(freq_sub())
  })



  ### 2c. Inconsistency
  
  make_Incon = function(freq) {
    incona<- netsplit(freq$net1)
    make_Incon<- netsplitresult.df(incona, input$modelranfix)
  }
  output$Incon1<- renderTable(colnames=TRUE, {
      make_Incon(freq_all())}
  )
  output$Incon2<- renderTable(colnames=TRUE, {
    make_Incon(freq_sub())}
  )

    
  
  

  #####################
  #### 3. Bayesian ####
  #####################
  
  
  
  ### SMD warninig alert
  
  observeEvent(list(input$baye_do,input$sub_do, input$node,input$node_sub), {
    # if (input$outcomeCont=="SMD") {
    #   showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
    # }
    # else 
      if (input$outcomebina=="RD") {
      showNotification("Please note: Risk difference currently cannot be analysed in Bayesian analysis", type = "warning")
    }
    })
  
  
  
  ### Bayesian analysis

  model <- eventReactive(input$baye_do, {
      newData1 <- as.data.frame(data())
      # label <- ifelse("Binary"=="Continuous",input$listCont,input$listbina)
      # treat_list <- read.csv(text=label, sep = "\t")
      treat_list <- data_connected()$lb
      longsort2 <- dataform.df(newData1,treat_list,"Binary")    # inputting the data in long form
      outc <- ifelse ("Binary"=="Continuous",input$outcomeCont, input$outcomebina)
      baye(longsort2,treat_list,input$modelranfix, outc,"Binary", ref_alter()$ref_all )
    })
  
  model_sub <- eventReactive(input$sub_do, {
      newData1 <- as.data.frame(data())
      # label <- ifelse("Binary"=="Continuous",input$listCont,input$listbina)
      # treat_list <- read.csv(text=label, sep = "\t")
      treat_list <- data_connected()$lb
      longsort2 <- dataform.df(newData1,treat_list,"Binary" )
      long_sort2_sub <- filter(longsort2, !Study %in% input$exclusionbox)  # subgroup
      outc <- ifelse ("Binary"=="Continuous",input$outcomeCont, input$outcomebina)
      baye(long_sort2_sub,treat_list,input$modelranfix, outc,"Binary", ref_alter()$ref_sub)
    })
  

  ### 3a. Forest plot
  
  output$gemtc <- renderPlot({                  # forest plot
    gemtc::forest(model()$mtcRelEffects,digits=3, xlim=c(log(0.1), log(10)))
    title(paste("All studies: 
              Bayesian", model()$a, "consistency model forest plot results"))
  })
  output$gemtc_sub <- renderPlot({
    forest(model_sub()$mtcRelEffects,digits=3, xlim=c(log(0.1), log(10)))
    title(paste("Results with studies excluded: 
              Bayesian", model_sub()$a,"consistency model forest plot results"))
  })
  
  output$text_gemtc <-renderText({          # tau
    gemtctau(model())
  })
  output$text_gemtc_sub <-renderText({
    gemtctau(model_sub())
  })
  
  output$dic <- renderTable ({                  # DIC table
    model()$dic
  }, digits=3, rownames=TRUE, colnames=FALSE
  )
  output$dic_sub <- renderTable ({
    model_sub()$dic
  }, digits=3, rownames=TRUE, colnames=FALSE)

  

  
  ### 3b. comparison of all treatment pairs
  
  baye_comp <- function(baye){
    tbl <- relative.effect.table(baye$mtcResults)
    if (("Binary"=="Binary") & (input$outcomebina!="RD")) {
        tbl<-exp(tbl)
    } 
    as.data.frame(round(tbl, digits=2))
  }
  
  output$baye_comparison <- renderTable ({
    baye_comp(model())
  }, digits=2, rownames=TRUE, colnames = TRUE
  )
  output$baye_comparison_sub <- renderTable ({
    baye_comp(model_sub())
  }, digits=2, rownames=TRUE, colnames = TRUE
  )
  
  
  ### 3c. ranking table and chart
  
  output$prob <- renderTable ({     # ranking table
    prob <- as.data.frame(print(rank.probability(model()$mtcResults,
          preferredDirection=(if (input$rankopts=="good") -1 else 1))))  
                                     # Put this code here (rather than in the main model) since the ranking selection is not needed in the model. The ranking is a separate function after getting the model results. 
                                     # so users are free to change the 'desirable' / 'undesirable' radiobutton without re-running the model.
    names(prob)[1:ncol(prob)] <- paste("Rank ", 1:(ncol(prob)), sep="")
    prob
  }, digits=5, rownames=TRUE, colnames = TRUE
  )
  output$prob_sub <- renderTable ({
    prob <- as.data.frame(print(rank.probability(model_sub()$mtcResults,preferredDirection=
                                                   (if (input$rankopts=="good") -1 else 1)))) 
    names(prob)[1:ncol(prob)] <- paste("Rank ", 1:(ncol(prob)), sep="")
    prob
  }, digits=5, rownames=TRUE, colnames = TRUE
  )
  
  output$gemtc_rank <- renderPlot ({    # ranking chart
    mod_list <- model()
    prob <- as.data.frame(print(rank.probability(mod_list$mtcResults,preferredDirection=
                                                   (if (input$rankopts=="good") -1 else 1))))
    prjtitle <- "Ranking with all studies - network meta-analysis median rank chart"
    rankl <- rownames(prob)
    mtcRank2(prjtitle, mod_list$ntx, rankl, prob, bcolr=FALSE)
  })
  output$gemtc_rank_sub <- renderPlot ({
    mod_list=model_sub()
    prob <- as.data.frame(print(rank.probability(mod_list$mtcResults,preferredDirection=
                                                   (if (input$rankopts=="good") -1 else 1))))
    prjtitle <- "Ranking with studies excluded - network meta-analysis median rank chart"
    rankl <- rownames(prob)
    ntx <- nrow(prob)
    mtcRank2(prjtitle, ntx, rankl, prob, bcolr=FALSE)
  })
  
  
  
  ### 3d. nodesplit model
  
  model_nodesplit <- eventReactive(input$node, {
    newData1 <- as.data.frame(data())
    # label <- ifelse("Binary"=="Continuous",input$listCont,input$listbina)
    # treat_list <- read.csv(text=label, sep = "\t")
    treat_list <- data_connected()$lb
    longsort2 <- dataform.df(newData1,treat_list,"Binary")
    outc <- ifelse ("Binary"=="Continuous",input$outcomeCont, input$outcomebina)
    bayenode(longsort2,treat_list, input$modelranfix, outc,"Binary" )
  })
  output$node_table<- renderTable(colnames=TRUE, {
    model_nodesplit()
  })
  
  model_nodesplit_sub <- eventReactive(input$node_sub, {
    newData1 <- as.data.frame(data())
    # label <- ifelse("Binary"=="Continuous",input$listCont,input$listbina)
    # treat_list <- read.csv(text=label, sep = "\t")
    treat_list <- data_connected()$lb
    longsort2 <- dataform.df(newData1,treat_list,"Binary")
    longsort2_sub <- filter(longsort2, !Study %in% input$exclusionbox)
    outc <- ifelse ("Binary"=="Continuous",input$outcomeCont, input$outcomebina)
    bayenode(longsort2_sub,treat_list, input$modelranfix, outc,"Binary")
  })
  output$node_table_sub<- renderTable(colnames=TRUE, {
    model_nodesplit_sub()
  })

  
  
  ### 3e. Bayesian result details and gelman
  
  output$gemtc_results <- renderPrint ({             # Results details
    model()$sumresults
  })
  output$gemtc_results_sub <- renderPrint ({
    model_sub()$sumresults
  })
  
  output$gemtc_gelman <- renderPlot ({              # Gelman plots
    gelman.plot(model()$mtcResults)
  })
  output$gemtc_gelman_sub <- renderPlot ({
    gelman.plot(model_sub()$mtcResults)
  })

  

  
  ### 3f. Deviance 
  
  scat_plot = function(baye){   # ume scatter plot
    mod_list=baye
    x<-mtc.deviance({mod_list$mtcResults})
    c <- data.frame(x$dev.ab)
    outc <- ifelse ("Binary"=="Continuous",input$outcomeCont, input$outcomebina)
    umeplot.df(c,mod_list$mtcNetwork, mod_list$model, mod_list$outcome)
  }
  umeplot <- eventReactive(input$baye_do, {      # to prevent 
    scat_plot(model())$p
  })
  
  output$dev_scat <- renderPlotly({
    umeplot()
  })
  
  umeplot_sub <- eventReactive(input$sub_do, {
    scat_plot(model_sub())$p
  })
  output$dev_scat_sub <- renderPlotly({
    umeplot_sub()
  })
  
  stemplot <- function(baye) {   # stemplot
    mod_list=baye
    x<-mtc.deviance({mod_list$mtcResults})
    c <- data.frame(x$dev.ab)
    c$names <- rownames(c)
    p<-stemplot.df(c,x)
  }
  output$dev1 <- renderPlotly({
    stemplot(model())
  })
  output$dev1_sub <- renderPlotly({
    stemplot(model_sub())
  })
  
  levplot <- function(baye) {    # leverage plot
    mod_list=baye
    x<-mtc.deviance({mod_list$mtcResults})
    p<-levplot.df(x)
  }
  output$dev2 <- renderPlotly({
    levplot(model())
  })
  output$dev2_sub <- renderPlotly({
    levplot(model_sub())
  })
  
  
  ### 3g.1 Model codes
  output$code <- renderPrint({
    cat(model()$mtcResults$model$code, fill=FALSE, labels=NULL, append=FALSE)
  })
  

  ### 3g.2 initial values
  output$inits <- renderPrint({
    model()$mtcResults$model$inits
  })
  
  ### 3g.3 download codes are in the download file.
  
  ### 3g.4 output deviance
  output$dev <- renderPrint({
    mtc.deviance({model()$mtcResults})
  })
  output$dev_sub <- renderPrint({
    mtc.deviance({model_sub()$mtcResults})
  })
  output$dev_ume<- renderPrint({
    scat_plot(model())$y
  })
  output$dev_ume_sub<- renderPrint({
    scat_plot(model_sub())$y
  })
  
  ######## User Guide ########
  output$UG <- downloadHandler(
    filename <- function() {
      paste("MetaInsightUserGBayv0.1","pdf", sep = ".")
    },
    content <- function(file){
      file.copy("./MetaInsightUserGBayv0_1.pdf", file)
    }
  )
  
})




