###### MetaInsight: COVID UI ######

#install.packages(c("dplyr","metafor", "netmeta","shiny", "shinyAce","rmarkdown", "knitr", "shinydashboard", "gemtc"
#  , "shinyalert", "ggplot2", "plotly"))

# install.packages("pkgbuild")
# pkgbuild::has_build_tools()
# install.packages(c("remotes", "knitr"))
# remotes::install_github("audrey-b/BUGSnet@v1.0.3", upgrade = TRUE, build_vignettes = TRUE)
# devtools::install_github("audrey-b/BUGSnet@v1.0.3", upgrade = TRUE, build_vignettes = TRUE)

library(dplyr)
library(netmeta)
library(metafor)
library(shiny)
library(shinyAce)
library(rmarkdown)
library(knitr)
library(shinydashboard)
library(gemtc)
library(shinyalert)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

source("PlotFunctionsRKO.R", local = TRUE) # Plot functions
load("blank.rds") # Objects to store data for plot functions

shinyUI(navbarPage(id="meta",
  "MetaInsight", 
  header = singleton(tags$head(includeScript("google_analytics2.js"))),
  tabPanel(id="covid", "COVID19",
           tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
           tags$style(     # to make the Shiny interface to zoom out 75% as default on Browser when loading as it looks better.
           "
              body {
             -moz-transform: scale(0.75, 0.75); /* Moz-browsers */
             zoom: 0.75; /* Other non-webkit browsers */
             zoom: 75%; /* Webkit browsers */
             }
             "),   
          column(1),
          column(10,
          h2(tags$strong("MetaInsight: COVID 19 (Last data check:"), tags$strong(textOutput("time2", container=span)), tags$strong(")")),
          
          h4("Network Meta-Analysis of Pharmacological treatments for COVID 19: Tool for exploration, re-analysis, sensitivity analysis, and interrogation of data from living systematic reviews",style = "cursive;
                      font-weight: 500; line-height: 1.1;
                      color: #2196c4"),
          br(),
          p(tags$strong("Purpose of the tool: Where uncertainty remains regarding the 'combinability' of the studies, our tool provides all stakeholders the opportunity to explore the robustness to what are 
            inevitably subjective decisions that you will sometimes have to make. Where others also publish related analyses it may also help reconcile the causes of any 
            differences."), style = "cursive; color: #000000"),
          br(),
          actionLink("infoclick", "Please click here to view project information.", icon("arrow-right")),
          br(),
          br(),
          p("If you use this app, please cite it as:",
            tags$a(href="https://doi.org/10.1186/s12874-022-01507-x", "Xin, Y., Nevill, C.R., Nevill, J. et al. Feasibility study for interactive reporting of network meta-analysis: experiences from the development 
                   of the MetaInsight COVID-19 app for stakeholder exploration, re-analysis and sensitivity analysis from living systematic reviews. BMC Med Res Methodol 22, 26 (2022).")),
          p("Codes for this app are available on GitHub. Please click ", tags$a(href="https://github.com/CRSU-Apps/MetaInsightCovid", "here"), " to access."),
          br(),
          prettyRadioButtons("COVIDoutc","Please select your outcome of interest to load data:",
                             c("All cause mortality " = "mortality","Incidence of viral negative conversion" = "conversion", "Incidence of serious adverse events" = "sae"), 
                             animation = "pulse", status = "info", width = '400px', bigger = TRUE),
          
          br(),
          
          #### Upload your own updated version of the data
          
          p(tags$strong("Please note that this pilot project has ended and the default data were last extracted on 19th October 2020 from the COVID-NMA Initiative (", style="color:#2196c4; background-color: #FFFFFF"),
            tags$a(href="https://www.covid-nma.com/", icon("mouse-pointer"), tags$strong("covid-nma.com", style="color:#2196c4; background-color: #FFFFFF"),target="_blank"),
            tags$strong(")."), style="color:#2196c4; background-color: #FFFFFF"),  
          p("If you would like to use this tool to continue updating and analysing the covid data, please follow the steps below:"),
          p("1. Download the default data to act as a template (", tags$a(href="https://docs.google.com/spreadsheets/d/1yutijO8Kp-4xYJ1xyoybObS-srZABUgwctHBL3-rQVM/edit?usp=sharing", icon("mouse-pointer")), "Google sheet link);"),
          p("2. Add new studies, remove studies, alter the details of current studies, and save in a Google sheet (formatting guidance below);"),
          downloadButton('covid_instruction', "Download formatting guidelines"),
          br(),
          p("3. Set the sharing settings of the Google sheet to 'Anyone with this link can view' (or 'edit' if multiple team members will be working on the data). Copy and paste the sharing link into the textbox below:"),
          textAreaInput("caption", NULL, 
                        placeholder="Insert Google sheet sharing link here", width = "1000px"),
          br(),
          
          ## general guidance warning ##
          
          p(tags$strong("Please note the following:", style="color:#2196c4; background-color: #FFFFFF"), "Whilst MetaInsight COVID-19 aims to make synthesis of COVID-19 evidence usable to those that may have limited programming abilities, 
            it is important to acknowledge that this app presents complex analyses. Network meta-analysis (NMA) is not an easy or straightforward analysis to conduct, and users should appreciate the assumptions and limitations that come with NMA. 
            Therefore, we encourage users to use this app sensibly and ask that they do not use this app in isolation, but have proper statistical support within their research project."),
          
          br(),
          
          # Table of data #
          
          h3("Characteristics of the included studies for the selected outcome",style = "cursive;
                      font-weight: 500; line-height: 1.1; 
                      color: #FFFFFF;background-color: #2196c4"),
          br(),
          div(style = 'overflow-x: scroll',DT::dataTableOutput("COVIDtb",width = "100%")),
          
          #DT::dataTableOutput('COVIDtb'),
          
          p("* The studies that are labeled as 'Not yet' are excluded from the default analysis due to causing the network to be disconnected. 
            They can be included separately by manual study selection (i.e. clicking upon the study)."),
          p(span("Abbreviations:",style = "text-decoration:underline"),
            span("IFN:",style = "font-weight: 600"),"Interferon;", 
            span("LopRitIFNa:",style = "font-weight: 600"),"Lopinavir + Ritonavir + Interferon-alpha;", 
            span("LopRitRibIFNa:",style = "font-weight: 600"),"Lopinavir + Ritonavir + Ribavirin + Interferon-alpha;", 
            span("LopRitRibIFNb1b:",style = "font-weight: 600"),"Lopinavir + Ritonavir + Ribavirin + Interferon-beta-1b;",
            span("LopRitDarCobUmiIFNa:",style = "font-weight: 600"),"Lopinavir + Ritonavir or Darunavir/Cobicistat + Umifenovir + Interferon-alpha;", 
            span("HCQ:",style = "font-weight: 600"),"Hydroxychloroquine;", 
            span("HCQ_Azith:",style = "font-weight: 600"),"Hydroxychloroquine + Azithromycin;", 
            span("Human_umbcord_stemcell_inf:",style = "font-weight: 600"),"Human umbilical cord mesenchymal stem cell infusion;", 
            span("rhG_CSF:",style = "font-weight: 600"),"Recombinant human granulocyte colony-stimulating factor;", 
            span("Interferon_kappa_TFF2:",style = "font-weight: 600"),"Interferon-kappa + Trefoil-factor-2"),
          br(),
         #tableOutput("texttry"),
         #textOutput("ref_change_note"),
         bsCollapse(id = "selec",
                    bsCollapsePanel("View the included studies after filtering or individual selection (Click to open / hide this panel)",
                                    tags$style("#none {color:#2196c4;display:block; }"),
                                    textOutput("none"),
                                    br(),
                                    div(style = 'overflow-x: scroll',DT::dataTableOutput("seldt",width = "100%")),
                                    # DT::dataTableOutput('seldt'),
                                    tags$style("#included_upon_sel {color:#2196c4;display:block; }"),
                                    textOutput("included_upon_sel"),
                                    style = "warning")),
         
         
         h3("Network plot",style = "cursive;
              font-weight: 500; line-height: 1.1; 
              color: #FFFFFF;background-color: #2196c4"),
         #p("Please note: there exist two separate networks. The primary network is displayed below.
         #       If you would like to view the overall disconnected network, please click the link below."),  # INFO to update
         textOutput("overalldiscon"),
         uiOutput("seltbshow"),
         br(),
         bsCollapse(id = "collapse",
                    bsCollapsePanel("View the disconnected overall network (Click to open / hide this panel)", 
                                    plotOutput("networkall_dis", height = 1200, width = 1200),
                                    style = "warning")),
         br(),
         tags$style("#test_selectmore {font-size:20px;
               color:#2196c4;
               display:block; }"),
         textOutput("test_selectmore"),
         
        # actionButton("test11", "test11"),
        tags$style(type="text/css",    #hide all the error message.
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        # conditionalPanel(condition= "input.networkstyle_sub_COVID=='networkp1'",
        #                  plotOutput("netGraphUpdating_COVID")
        # ),
        # conditionalPanel(condition= "input.networkstyle_sub_COVID=='networkp2'",
        #                  plotOutput("netGraphUpdating_COVID",height = 1200, width = 1200 )
        # ),
                      
        plotOutput("netGraphUpdating_COVID", height = 700, width = 1200),
        p(span("Abbreviations:",style = "text-decoration:underline"),
          span("IFN:",style = "font-weight: 600"),"Interferon;", 
          span("LopRitIFNa:",style = "font-weight: 600"),"Lopinavir + Ritonavir + Interferon-alpha;", 
          span("LopRitRibIFNa:",style = "font-weight: 600"),"Lopinavir + Ritonavir + Ribavirin + Interferon-alpha;", 
          span("LopRitRibIFNb1b:",style = "font-weight: 600"),"Lopinavir + Ritonavir + Ribavirin + Interferon-beta-1b;",
          span("LopRitDarCobUmiIFNa:",style = "font-weight: 600"),"Lopinavir + Ritonavir or Darunavir/Cobicistat + Umifenovir + Interferon-alpha;", 
          span("HCQ:",style = "font-weight: 600"),"Hydroxychloroquine;", 
          span("HCQ_Azith:",style = "font-weight: 600"),"Hydroxychloroquine + Azithromycin;", 
          span("Human_umbcord_stemcell_inf:",style = "font-weight: 600"),"Human umbilical cord mesenchymal stem cell infusion;", 
          span("rhG_CSF:",style = "font-weight: 600"),"Recombinant human granulocyte colony-stimulating factor;", 
          span("Interferon_kappa_TFF2:",style = "font-weight: 600"),"Interferon-kappa + Trefoil-factor-2"),
         conditionalPanel(condition= "input.networkstyle_sub_COVID=='networkp1'",
                          fluidRow(
                            column(2),
                          column(8, p("The trials that contain zero outcome (i.e. missing treatment effect) on both arms are not displayed in this style. For the default analysis, trials that become disconnected after removal of such trials are also not displayed. 
                                      Please switch to style 2 to view the full network.",
                            style = "background-color: #ffd966")),
                          column(2)
                          )
                          ),
         conditionalPanel(condition ="input.networkstyle_sub_COVID=='networkp1' && input.COVIDoutc!='sae'",
                          p("Numbers on the line indicate the number of trials conducted for the comparison. The shaded areas (if there are any) indicate there exist multi-arm trials between the comparisons."),
                          p("This style cannot display disconnected network. If the network is disconnected, please switch to style 2 to view.")
         ),
         conditionalPanel(condition= "input.networkstyle_sub_COVID=='networkp1' && input.COVIDoutc=='sae'",
                          fluidRow(
                            column(1),
                            column(10,
                                   tags$style("#text_sea_disp {display:block; background-color:#ffd966 }"),
                                   textOutput("text_sea_disp"),
                          #p("Please note that if the network is not shown in this style, it could be due to the following reasons: a) The frequentist analysis automaticlaly 
                          #  removes the study arms that contain 0 event in multi-arm trials so for example if Yueping et al is included, it will be left with a single arm, causing the network not proceeded; b) The network is disconnected. In either case, please switch to style 2 to view the network.",
                          #  style = "background-color: #ffd966")
                          ),
                          column(1)
         )),
         conditionalPanel(condition= "input.networkstyle_sub_COVID=='networkp2'",
                          p("The size of the nodes and the thickness of edges depend on the number of people randomised and the number of trials conducted, respectively.")
         ),
         radioButtons("networkstyle_sub_COVID", "Please choose a network plot style", c("Number of trials shown on the line" = "networkp1","Number of people indicated by size of node etc." = "networkp2"), selected = "networkp1"),

         h3(" Forest plot result", style = "cursive;
              font-weight: 500; line-height: 1.1; 
              color: #FFFFFF;background-color: #2196c4"),
         br(),
        p("The forest plot is from a frequentist analysis where trials that contain zero outcome (i.e. missing treatment effect) on both arms are not analysed. 
          For the default analysis, trials that become disconnected after removal of such trials are also not analysed. Bayesian analysis options are available 
          on the ", actionLink("detailedANclick", tags$strong("'Detailed data analysis'"))),
         br(),
         plotOutput("freq_sub_covid", height = 600, width = 1200),
        p(span("Abbreviations:",style = "text-decoration:underline"),
          span("IFN:",style = "font-weight: 600"),"Interferon;", 
          span("LopRitIFNa:",style = "font-weight: 600"),"Lopinavir + Ritonavir + Interferon-alpha;", 
          span("LopRitRibIFNa:",style = "font-weight: 600"),"Lopinavir + Ritonavir + Ribavirin + Interferon-alpha;", 
          span("LopRitRibIFNb1b:",style = "font-weight: 600"),"Lopinavir + Ritonavir + Ribavirin + Interferon-beta-1b;",
          span("LopRitDarCobUmiIFNa:",style = "font-weight: 600"),"Lopinavir + Ritonavir or Darunavir/Cobicistat + Umifenovir + Interferon-alpha;", 
          span("HCQ:",style = "font-weight: 600"),"Hydroxychloroquine;", 
          span("HCQ_Azith:",style = "font-weight: 600"),"Hydroxychloroquine + Azithromycin;", 
          span("Human_umbcord_stemcell_inf:",style = "font-weight: 600"),"Human umbilical cord mesenchymal stem cell infusion;", 
          span("rhG_CSF:",style = "font-weight: 600"),"Recombinant human granulocyte colony-stimulating factor;", 
          span("Interferon_kappa_TFF2:",style = "font-weight: 600"),"Interferon-kappa + Trefoil-factor-2"),
        
         p("If this plot does not display fully, please zoom out on your Browser."),
        tags$style("#text_freq_disp {display:block; background-color:#ffd966 }"),
        textOutput("text_freq_disp"),
         br(),
         p(tags$strong("Interested in conducting detailed analysis? For example, changing the analysis model, estimating the ranking, exploring inconsistency, etc.? Please go to the"),icon("hand-point-right"), actionLink("detailedANclick", tags$strong("
            'Detailed data analysis'")),tags$strong("page.")),
         p(tags$strong("Want to upload your own data and make use of the full flexibility of MetaInsight?
           Please go to our regular version of the app:"), icon("mouse-pointer"),
           tags$a(href="https://crsu.shinyapps.io/metainsight", tags$strong("https://crsu.shinyapps.io/metainsight"),target="_blank"),
           tags$strong(".")),
         br(),
         br(),
         wellPanel(
           fluidRow(
             column(5, img(src='NIHR_Logo4.jpg', width=472, height=125)),
             # column(3, ),
             column(7, tags$div(class="header", checked=NA,
                                tags$strong("Funding Acknowledgement:"),
                                tags$p("The Complex Reviews Support Unit is funded by the National Institute for Health Research (project number 14/178/29)."),
                                tags$strong("Department of Health Disclaimer: "),
                                tags$p("The views and opinions expressed herein are those of the authors and do not necessarily reflect those of the NIHR, NHS or the Department of Health."),
                                tags$p("Please click ", tags$a(href="http://www.nihrcrsu.org", "here ",
                                                               target="_blank"), "for more information about the UK NIHR Complex Reviews Support Unit (CRSU).")
             ))))
          )
         ),

 tabPanel(id="Intro", "Project introduction"
          ,
          column(1),
          column(10,
                 h2(tags$strong("Project Introduction")),
                 br(),
                 h4("Why was this tool made and what does it do?",style = "cursive;
                      font-weight: 500; line-height: 1.1;
                      color: #FFFFFF;background-color: #2196c4"),
                 br(),
                 p("MetaInsight is an interactive web-based tool that conducts network meta-analysis requiring no specialist software for the user to install but leveraging established
                 analysis routines (specifically, but not exclusively, the",
                   tags$a(href="https://cran.r-project.org/web/packages/netmeta/index.html", "netmeta 1.2-1",target="_blank"),
                   tags$a(href="https://cran.r-project.org/web/packages/gemtc/index.html", "gemtc 0.8-4",target="_blank"),
                   tags$a(href="https://bugsnetsoftware.github.io/", "BUGSnet",target="_blank"),
                   "in R) (",
                   tags$a(href="https://onlinelibrary.wiley.com/doi/full/10.1002/jrsm.1373", "Owen et al. 2019",target="_blank"),
                   ").The regular version of the app can be found elsewhere (",
                   tags$a(href="https://crsu.shinyapps.io/metainsight", "https://crsu.shinyapps.io/metainsight",target="_blank"),
                   ")."),
                 p("With the evidence on treating COVID19 rapidly evolving, we have created a special version of our app - MetaInsight: COVID19 - tailored specifically around the up-to-date 
                   evidence from randomised controlled trials on the effectiveness of pharmacological treatments for COVID19. It is our intention to update the data included in this app regularly 
                   (the hope is this can be done at least weekly) as it becomes available. As well as updating the data, we will be increasing the functionality of the tool as well periodically 
                   (see below on providing feedback into this process). "),
                 p("This tool is designed to act as a dynamic, and interactive tool for analysis of network meta-analysis with an emphasis on visualisation of data and analysis results. 
                   In every meta-analysis, subjective judgements have to be made including the studies and data included and the analysis method used. As others have noted (",
                   tags$a(href="https://ebm.bmj.com/content/early/2020/04/09/bmjebm-2019-111308.info", "Ahern et al. 2020",target="_blank"),
                   "), traditional static evidence summaries force the quality assessment criteria and analytical choices of the authors onto all stakeholders, some of whom may have different views 
                   on key features of the analysis. Through tools such as this, users can, with ease, take control of the evidence synthesis using their preferred analytic approach to ascertain 
                   how robust findings are to alternative analysis strategies and study inclusion criteria and assess the appropriateness of the modelling assumptions made etc."),
                 br(),
                 h4("Where does the data come from?",style = "cursive;
                      font-weight: 500; line-height: 1.1;
                      color: #FFFFFF;background-color: #2196c4"),
                 br()
                 ,
                 p("The authors of the MetaInsight:COVID19 tool are not involved in conducting a systematic review or meta-analysis of the COVID19 treatment evidence of their own, but several
                   other groups around the world currently are carrying out living systematic reviews of this topic and frequently updating the evidence on their websites (",
                   tags$a(href="https://covid-nma.com/", "COVID19 - Living data",target="_blank"),
                   ",",
                   tags$a(href="http://eppi.ioe.ac.uk/cms/Projects/DepartmentofHealthandSocialCare/Publishedreviews/COVID-19Livingsystematicmapoftheevidence/tabid/3765/Default.aspx", "EPPI Center",target="_blank"),
                   ", and",
                   tags$a(href="https://systematicreviewsjournal.biomedcentral.com/articles/10.1186/s13643-020-01371-0", "The LIVING Project",target="_blank"),
                   "). Presently, we include data from one of these (",
                   tags$a(href="https://covid-nma.com/", "COVID19 - Living data https://covid-nma.com/",target="_blank"),
                   "), though, depending on how each of these evolve, we may include data from other sources in the future."),
                 p("Presently we are focusing solely on evidence from randomised controlled trials, but observational data may be considered in the future. "),
                 br(),
                 h4("How does the app work?",style = "cursive;
                      font-weight: 500; line-height: 1.1;
                      color: #FFFFFF;background-color: #2196c4"),
                 br()
                 ,
                 p("The MetaInsight: COVID19 tool contains a first page titled 'COVID19' which includes functionality for outcome selection. Following this, the corresponding
                   evidence table, network plot and results (shown as a summary forest plot) for this outcome are displayed. On subsequent pages the functionality of the pre-existing
                   MetaInsight app is included to explore the data and do detailed re-analyses. For example, specific studies can be filtered and excluded from the analysis, the analysis model
                   changed, and assessments of network coherence carried out. We hope much of the functionality is self-explanatory, but a full manual is available on the",
                   actionLink("manualclick", "User Manual"),"tab.")
                 ,
                 p("The advanced section of the app allows the user to fit NMA models via Bayesian simulation methods using the external package",
                   tags$a(href="https://en.wikipedia.org/wiki/Just_another_Gibbs_sampler", "JAGS",target="_blank"),
                   ". In addition to technical advantages,
                   this also allows the checking of modelling assumptions via cross validation and the use of model fit statistics to choose between competing statistical models. The app also
                   allows the user to download the R code used 'under the hood' that carried out the analysis to ensure reproducibility of research.")
                 ,
                 br()
                 ,
                 h4("About us",style = "cursive;
                      font-weight: 500; line-height: 1.1;
                      color: #FFFFFF;background-color: #2196c4"),
                 br(),
                 p("MetaInsight was developed by the UK National Institute for Health Research (NIHR) Complex Review Support Unit (",
                   tags$a(href="http://www.nihrcrsu.org/", "http://www.nihrcrsu.org/",target="_blank"),
                   "). We routinely support network meta-analyses and developed (peer reviewed) web-based software (including MetaInsight -",
                   tags$a(href="http://www.nihrcrsu.org/guidance/apps/", "http://www.nihrcrsu.org/guidance/apps/",target="_blank"),
                   ") to allow others, including Cochrane Collaboration groups, to conduct and critique network meta-analyses without the need for expert statistics programming."),
                 p("We have received excellent feedback on MetaInsight and know it is getting 100s of hours use each month by researchers in many areas of the world,
                   including those in low and middle income countries. In addition to its primary function of allowing non-statistical experts to do research using NMA,
                   we get enthusiastic reports from statisticians who use it because of its ease and efficiency and educators who use it as a practical teaching tool in the classroom.
                   The Cochrane Collaboration (www.cochrane.org), whose work is internationally recognised as a benchmark for high-quality information about the effectiveness of healthcare,
                   promoted the use of MetaInsight recently via a webinar (",
                   tags$a(href="https://training.cochrane.org/resource/metainsight-complex-review-support-unit-crsu-network-meta-analysis-nma-web-based-app", "https://training.cochrane.org/resource/metainsight-complex-review-support-unit-crsu-network-meta-analysis-nma-web-based-app",target="_blank"),
                   ")."
                   ),
                 br(),
                 h4("Contributors",style = "cursive;
                      font-weight: 500; line-height: 1.1;
                      color: #FFFFFF;background-color: #2196c4"),
                 br(),
                 p("Yiqiao Xin",tags$sup("1"), ", Clareece Nevill",tags$sup("2"),", Janion Nevill", ", Ewan Gray", tags$sup("3"),", Nicola Cooper", tags$sup("2"),", Rhiannon K Owen", tags$sup("2"),", Naomi Bradbury", tags$sup("4"),  ", and Alex Sutton", tags$sup("2"), "."),
                 p(tags$strong("Steering committee")),
                 p("Olivia Wu", tags$sup("1"), ", Neil Hawkins", tags$sup("1"),", Suzanne Freeman", tags$sup("2"),", Peter Langhorne",tags$sup("5"),", Terry Quinn", tags$sup("5"),",  and Moira Sim",tags$sup("1"),),
                 p(tags$strong("Affiliations")),
                 p("1 NIHR Complex Review Support Unit, Health Technology Assessment and Health Economics (HEHTA), Institute of Health and Wellbeing, University of Glasgow, UK. "),
                 p("2 NIHR Complex Review Support Unit, Department of Health Sciences, University of Leicester, UK."),
                 p("3 Edinburgh Clinical Trials Unit (ECTU), University of Edinburgh, UK."),
                 p("4 Zeeman Institute: Systems Biology and Infectious Disease Epidemiology Research (SBIDER), School of Life Sciences, University of Warwick, Coventry, UK."),
                 p("5 Institute of Cardiovascular & Medical Sciences, University of Glasgow, UK."),
                 br(),
                 h4("Feedback and Referencing",style = "cursive;
                      font-weight: 500; line-height: 1.1;
                      color: #FFFFFF;background-color: #2196c4"),
                 br(),
                 p("This is a novel and evolving project. We hope similar tools can be made for other important living and completed network meta-analyses in the future. 
                 We would be very grateful to anyone who sends constructive feedback and suggestions on how the tool can be improved in the future and this will inform future 
                 versions of this and any subsequent tools. Please send any feedback to",
                  tags$a(href="mailto:clareece.nevill@le.ac.uk", "clareece.nevill@le.ac.uk",target="_blank"), "."),
                 br(),
                 p("If you use this app, please cite it as:",
                   tags$a(href="https://doi.org/10.1186/s12874-022-01507-x", "Xin, Y., Nevill, C.R., Nevill, J. et al. Feasibility study for interactive reporting of network meta-analysis: experiences from the development 
                   of the MetaInsight COVID-19 app for stakeholder exploration, re-analysis and sensitivity analysis from living systematic reviews. BMC Med Res Methodol 22, 26 (2022).")),
                 p("Codes for this app are available on GitHub. Please click ", tags$a(href="https://github.com/CRSU-Apps/MetaInsightCovid", "here"), " to access."),
                br(),
                br(),
                br(),
                p("THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
                 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
                 IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
                 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
                 OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."),
                br(),
                wellPanel(
                 fluidRow(
                   column(5, img(src='NIHR_Logo4.jpg', width=472, height=125)),
                   column(7, tags$div(class="header", checked=NA,
                      tags$strong("Funding Acknowledgement:"),
                      tags$p("The Complex Reviews Support Unit is funded by the National Institute for Health Research (project number 14/178/29)."),
                      tags$strong("Department of Health Disclaimer: "),
                      tags$p("The views and opinions expressed herein are those of the authors and do not necessarily reflect those of the NIHR, NHS or the Department of Health."),
                      tags$p("Please click ", tags$a(href="http://www.nihrcrsu.org", "here ",
                                                     target="_blank"), "for more information about the UK NIHR Complex Reviews Support Unit (CRSU).")
                )))),
          
          ),
          column(1)

),
           


#############################
### Tab 3 - Data analysis ###
#############################


tabPanel("Detailed data analysis",
     htmlOutput("CONBI2"),
     tags$head(tags$style("#CONBI2{color: white;
               font-size: 20px;
               font-style: bold;
               background-color: #2196c4
               }"
     )),
     br(),
     sidebarLayout(
       sidebarPanel(
         # conditionalPanel(condition= "input.metaoutcome=='Continuous'",
         #  radioButtons("outcomeCont", "Outcome for continuous data:", c("Mean Difference (MD)" = "MD","Standardised Mean Difference (SMD)" = "SMD"))
         # ),
        # conditionalPanel(condition = "input.metaoutcome=='Binary'",
          radioButtons("outcomebina", "Outcome for binary data:", c("Odds Ratio (OR)" = "OR","Risk Ratio (RR)" = "RR", "Risk Difference (RD)" = "RD"))
        # )
        ,
        #  radioButtons('rankopts', 'For treatment rankings, smaller outcome values
        #               (e.g. smaller mean values for continuous data, or in some cases,
        #               e.g. log ORs, more negative values, for binary data) are:',
        #               c("Desirable" = "good", "Undesirable" = "bad")
        # #              , selected = "good"
        #               ),
        uiOutput("rankbutton"),
         radioButtons("modelranfix", "Model:", c("Random effect (RE)" = "random", "Fixed effect (FE)" = "fixed")),
        br(),
         h3("Select studies to exclude:"),
        p("Tips: You can use the study characterstics table to find the study that you want to exclude."),
        actionButton("datatablebutton", "Open the data table", icon("angle-double-right")),
        br(),
        br(),
        uiOutput("Choicesexcl"), h5("NB: If a whole treatment is removed from the analysis the NMA will return an error message. To overcome this, please remove the treatment from the data."), width = 3
       ),
     mainPanel(
       bsCollapse(id = "collapse2", 
                  bsCollapsePanel("View data (Click to open / hide this panel)",
                                  h4("Characteristics of the included studies for the selected outcome", style = "cursive;
                      font-weight: 300; line-height: 1; 
                      color: #2196c4;background-color:#FFFFFF "),
                                  p("Users can use the filter box under each column of heading to select studies to exclude in the sensitivity analysis."),
                                  div(style = 'overflow-x: scroll',DT::dataTableOutput("datatb",width = "100%")),
                                  #DT::dataTableOutput('datatb'),
                                  style = "warning")),
       tags$style(HTML("
            .tabbable > .nav > li > a                  {background-color: white;  color:#2196c4}
            .tabbable > .nav > li > a[data-value='1. Data summary'] {background-color: #2196c4;  color:white; font-size: 18px}
            .tabbable > .nav > li > a[data-value='1a. Study Results'] {background-color: white;}
            .tabbable > .nav > li > a[data-value='1b. Network Plot'] {background-color: white;}
            .tabbable > .nav > li > a[data-value='2. Frequentist network meta-analysis'] {background-color: #2196c4;   color:white; font-size: 18px}
            .tabbable > .nav > li > a[data-value='2a. Forest Plot'] {background-color: white}
            .tabbable > .nav > li > a[data-value='2b. Comparison of all treatment pairs'] {background-color: white;}
            .tabbable > .nav > li > a[data-value='2c. Inconsistency'] {background-color: white;}
            .tabbable > .nav > li > a[data-value='3. Bayesian network meta-analysis'] {background-color: #2196c4;   color:white; font-size: 18px}
            .tabbable > .nav > li[class=active]    > a {font-weight:900;font-style: italic;text-decoration: underline }
            ")),
       tabsetPanel(
         tabPanel("1. Data summary", tabsetPanel(
           tabPanel("1a. Data Characteristics", 
                    p("This tab shows a summary of study characteristics."),
                    column(6,
                           h4("Characteristics table of all studies"),
                           tableOutput("sumtb")
                    ),
                    column(6,
                           h4("Characteristics table with studies excluded"),
                           tableOutput("sumtb_sub")
                    )
           ),
            # tabPanel("1b. Study Results", 
            #          plotOutput("forestPlot", height = "1000px", width = "800px"), 
            #          radioButtons('format_freq0', 'Document format', c('PDF', 'SVG'), inline = TRUE),  downloadButton('downloadStudy')),
            tabPanel("1b. Network Plot",
              column(6, 
                     plotOutput("netGraphStatic"),
                     conditionalPanel(condition= "input.networkstyle=='networkp1'",
                                      p("The trials that contain zero outcome (i.e. missing treatment effect) on both arms are not displayed in this style.
                                        For the default analysis, trials that become disconnected after removal of such trials are also not displayed.",
                                                  style = "background-color: #ffd966"),
                                      p("Numbers on the line indicate the number of trials conducted for the comparison. The shaded areas indicate there exist multi-arm trials between the comparisons.")
                     ),
                     conditionalPanel(condition= "input.networkstyle=='networkp2'",
                                      p("The size of the nodes and the thickness of edges depend on the number of people randomised and the number of trials conducted, respectively.")
                     ),
                     radioButtons("networkstyle", "Please choose a network plot style", c("Number of trials shown on the line" = "networkp1","Number of people indicated by size of node etc." = "networkp2"), selected = "networkp1"),
                     radioButtons('format_freq1', 'Document format', c('PDF', 'PNG'), inline = TRUE),
                     downloadButton('downloadNetwork'), 
                     br(),
                     br(),
                     br(),
                     #verbatimTextOutput("netconnect")
                     ),
              column(6, plotOutput("netGraphUpdating"),
                     conditionalPanel(condition= "input.networkstyle_sub=='networkp1'",
                                      p("The trials that contain zero outcome (i.e. missing treatment effect) on both arms are not displayed in this style.",
                                        style = "background-color: #ffd966"),
                                      p("Numbers on the line indicate the number of trials conducted for the comparison. The shaded areas indicate there exist multi-arm trials between the comparisons.")
                     ),
                     conditionalPanel(condition= "input.networkstyle_sub=='networkp2'",
                                      p("The size of the nodes and the thickness of edges depend on the number of people randomised and the number of trials conducted, respectively.")
                     ),
                     radioButtons("networkstyle_sub", "Please choose a network plot style", c("Number of trials shown on the line" = "networkp1","Number of people indicated by size of node etc." = "networkp2"), selected = "networkp1"),
                     radioButtons('format_freq2', 'Document format', c('PDF', 'PNG'), inline = TRUE),
                     downloadButton('downloadNetworkUpdate'), 
                     br(),
                     br(),
                     br(),
                     #verbatimTextOutput("netconnect_sub")
                     )
            )
  ###### bugsnet new code ########         
            # ,
            # tabPanel("1d. Covariates plot", 
            #          p("this is from the TSD rheumatoid arthritis dataset, as the example data do not have covariates "),
            #          plotOutput("covp"))
  ###### finish ########
         )),
         tabPanel("2. Frequentist network meta-analysis", tabsetPanel(
            tabPanel("2a. Forest Plot",
                     conditionalPanel(condition= "input.COVIDoutc!='conversion'",
                                      fluidRow(
                                        column(1),
                                        column(9, p("The trials that contain zero outcome (i.e. missing treatment effect) on both arms are not analysed in the frequentist analysis.
                                                    For the default analysis, trials that become disconnected after removal of such trials are also not analysed.",
                                                    style = "background-color: #ffd966")),
                                        column(2)
                                      )
                     ),
                column(6, plotOutput("Comparison2"
                                     , height = 800
                                     ), textOutput("textcomp"), textOutput("ref4"), radioButtons('format_freq5', 'Document format', c('PDF', 'PNG'), inline = TRUE), downloadButton('downloadfreqfor')
                  ),
                column(6, plotOutput("SFPUpdatingComp"
                                     , height = 800
                                     ), textOutput("text5"), textOutput("ref3"), radioButtons('format_freq6', 'Document format', c('PDF', 'PNG'), inline = TRUE), downloadButton('downloadfreqfor2'))
                 ),
            tabPanel("2b. Comparison of all treatment pairs",
                     conditionalPanel(condition= "input.COVIDoutc!='conversion'",
                                      fluidRow(
                                        column(1),
                                        column(9, p("The trials that contain zero outcome (i.e. missing treatment effect) on both arms are not analysed in the frequentist analysis.",
                                                    style = "background-color: #ffd966")),
                                        column(2)
                                      )
                     ),
                helpText("Treatments are ranked from best to worst along the leading diagonal. Above the leading diagonal are estimates from pairwise meta-analyses, below the leading diagonal are estimates from network meta-analyses"),
                helpText("Relative treatment effects in ranked order for all studies"), tableOutput("rankChartStatic"), downloadButton('downloadRank', "Download"),
                helpText("Relative treatment effects in ranked order with studies excluded"), tableOutput("rankChartUpdating"), downloadButton('downloadRankUpdate')),
            tabPanel("2c. Inconsistency", 
                     conditionalPanel(condition= "input.COVIDoutc!='conversion'",
                                      fluidRow(
                                        column(1),
                                        column(9, p("The trials that contain zero outcome (i.e. missing treatment effect) on both arms are not analysed in the frequentist analysis.",
                                                    style = "background-color: #ffd966")),
                                        column(2)
                                      )
                     ),
                helpText("Assessment of inconsistency for all studies"), 
                tableOutput("Incon1"), downloadButton('downloadIncon', "Download"),
                helpText("Assessment of inconsistency with studies excluded"), 
                tableOutput("Incon2"), downloadButton('downloadIncon2', "Download")
            ))),
       tabPanel("3. Bayesian network meta-analysis", tabsetPanel(id="tab",
         tabPanel("3a. Forest plot",
            helpText("Baysesian result using the gemtc package.", tags$strong("Please note each simulation may take 20 seconds. If the plots does not display fully, please zoom out on your Browser.", style="color:#FF0000")),
            fixedRow(
              column(6, align = "center",
                     p(tags$strong("Results for all studies")),
                     p("Please click the button below to run Bayesian analysis for all studies, and after each time when you change the radiobutton selections."),
                     actionButton("baye_do", "Click here to run the main analysis for all studies")
              ),
              column(6, align = "center",
                     p(tags$strong("Results with studies excluded")),
                     p("Please click the button below to run each time after you finish the selection of studies, or change the radiobutton selections."),
                     actionButton("sub_do", "Click here to run the sensitivity analysis")
              )),
            fixedRow(
              column(6, align = "center",
                     plotOutput("gemtc", height = 800),
                     p("Model fit:"),
                     tableOutput("dic"),
                     textOutput("text_gemtc"),
                     br(),
                     br(),
                     radioButtons('format2', 'Document format', c('PDF', 'PNG'), inline = TRUE), 
                     downloadButton('downloadBaye_plot')
              ),
              column(6, align = "center",
                     plotOutput("gemtc_sub", height = 800),
                     p("Model fit:"),
                     tableOutput("dic_sub"),
                     textOutput("text_gemtc_sub"),
                     br(),
                     br(),
                     radioButtons('format4', 'Document format', c('PDF', 'PNG'), inline = TRUE), 
                     downloadButton('downloadBaye_plot_sub')
              ))),
       tabPanel("3b. Comparison of all treatment pairs",
            helpText("Please note: if you change the selections on the sidebar, 
                               you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
            p(tags$strong("In contrast to the 'comparison of all treatment pairs' tab in the frequentist NMA results, 
              this table only contains the estimates from the network meta analysis, 
              i.e. does not contain estimates from pairwise meta-analysis which only contains direct evidence. 
                          If you would like to obtain the pairwise meta-analysis results, please run 3d. Nodesplit model")),
            br(),
            p(tags$strong("Treatment effects for all studies: comparison of all treatment pairs.")),
            tableOutput("baye_comparison"),
            downloadButton('downloadbaye_comparison'),
            br(),
            br(),
            p(tags$strong("Treatment effects with studies excluded: comparison of all treatment pairs.")),
            tableOutput("baye_comparison_sub"),
            downloadButton('downloadbaye_comparison_sub')
              ),
       tabPanel("3c. Ranking table",
                helpText("Please note: if you change the selections on the sidebar, 
                               you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
               p(tags$strong("Ranking table for all studies - Probability for each treatment to be the best")),
               div(tableOutput("prob"), style = "font-size:100%"),
               downloadButton('downloadBaye_rank'),
               #plotOutput("gemtc_rank"),
               p(tags$strong("Ranking table with studies excluded - Probability for each treatment to be the best")),
               div(tableOutput("prob_sub"), style = "font-size:100%"),
               downloadButton('downloadBaye_rank_sub')
               #plotOutput("gemtc_rank_sub")
              ),
      tabPanel("3d. Nodesplit model", 
           p("Please note: This may take more than 10 minutes depending on the number of treatment options. The node splitting option for
           the Bayesian analysis is highly numerically intensive and using it on the app can cause the app to disconnect in some circumstances.  
               we recommend users to download the whole app through ",
               tags$a(href="https://rstudio.cloud/project/1029951", "R Cloud", target="_blank"),
               "or ", tags$a(href="https://github.com/yiqiaoxin/CRSU-MetaInsight/", "Github",target="_blank"), 
               "and run it locally through RStudio if they want to make use of this function."),
           fluidRow(
             column(6,
                    p(tags$strong("Inconsistency test with notesplitting model for all studies")),
                    actionButton("node", "Click here to run the nodesplitting analysis for all studies"),
                    tableOutput("node_table"),
                    downloadButton('downloadnode')
             ),
             column(6,
                    p(tags$strong("Inconsistency test with notesplitting model with studies excluded")),
                    actionButton("node_sub", "Click here to run the nodesplitting analysis with studies excluded"),
                    tableOutput("node_table_sub"),
                    downloadButton('downloadnode_sub')
             ))),
      tabPanel("3e. Bayesian result details",
               helpText("Please note: if you change the selections on the sidebar, 
                               you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
          fluidRow(
             column(6,
                p(tags$strong("Results details for all studies")),
                verbatimTextOutput("gemtc_results"),
                p(tags$strong("Gelman convergence assessment plot for all studies")),
                plotOutput("gemtc_gelman")
                ),
            column(6,
                p(tags$strong("Results details with studies excluded")),
                verbatimTextOutput("gemtc_results_sub"),
                p(tags$strong("Gelman convergence assessment plot with studies excluded")),
                plotOutput("gemtc_gelman_sub"))
            )),        
      tabPanel("3f. Deviance report",
          p(tags$strong("Deviance report for all studies and the sensitivity analysis")),
          helpText("Please note: if you change the selections on the sidebar, 
                               you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
          fluidRow(
           column(6,
                  p(tags$strong("Residual deviance from NMA model and UME inconsistency model for all studies")),
                  plotlyOutput("dev_scat")),
           column(6,
                  p(tags$strong("Residual deviance from NMA model and UME inconsistency model with studies excluded")),
                  plotlyOutput("dev_scat_sub")
           )),
           p("This plot represents each data points' contribution to the residual deviance for the 
          NMA with consistency (horizontal axis) and the unrelated mean effect (ume) inconsistency models 
          (vertical axis) along with the line of equality. The points on the equality line means there is no
          improvement in model fit when using the inconsistency model, suggesting that there is no evidence of inconsistency. 
          Points above the equality line means they have a smaller residual deviance for the consistency model indicating a 
          better fit in the NMA consistency model and points below the equality line
          means they have a better fit in the ume inconsistency model. Please note that the unrelated mean effects model 
          may not handle multi-arm trials correctly. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for 
            decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.)"),
           br(),
           br(),
           br(),
           fluidRow(
             column(6,
                    p(tags$strong("Per-arm residual deviance for all studies")),
                    plotlyOutput("dev1")),
             column(6,
                    p(tags$strong("Per-arm residual deviance for sensitivity analysis")),
                    plotlyOutput("dev1_sub")
             ),
             br(),
           p("This stem plot represents the posterior residual deviance per study arm. The total number of stems equals 
             to the total number of data points in the network meta analysis. Each stem is corresponding to each arm in 
             each study in the deviance results below ($dev.ab) (through which you can identify which stem corresponds 
             to which study arm). The smaller residual deviance (the shorter stem), dev.ab, the better model fit for each 
             data point. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for 
             decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.)"),
           br(),
           br(),
           br(),
           column(6,
                  p(tags$strong("Leverage plot for all studies")),
                  plotlyOutput("dev2")
           ),
           column(6,
                  p(tags$strong("Leverage plot for sensitivity analysis")),
                  plotlyOutput("dev2_sub"))
           ),
           br(),
           p("This leverage plot shows the average leverage of the data point across the arms for each study 
           (sum($lev.ab) for each study/number of arms for each study) versus the square root of the average residual 
             deviance across the arms for each study (sum($dev.ab) for each study / number of arms for each study). 
             The leverage for each data point, is calculated as the posterior mean of the residual 
             deviance, minus the deviance at the posterior mean of the fitted values. The leverage plot may be used to 
             identify influential and/or poorly fitting studies and can be used to check how each study is affecting 
             the overall model fit and DIC. Curves of the form x2 + y = c, c = 1, 2, 3, ., where x represents square root 
             of residual deviance, and y represents the leverage, are marked on the plot. Points lying on such parabolas 
             each contribute an amount c to the DIC (Spiegelhalter et al., 2002). Points that lie outside the line with 
             c = 3 can generally be identified as contributing to the model's poor fit. Points with a high leverage are 
             influential, which means that they have a strong influence on the model parameters that generate their fitted 
             values. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for 
             decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd. 
             Spiegelhalter et al. (2002) Bayesian measures of model complexity and fit. J. R. Statist. Soc.B 64, Part4, 
             pp.583-639)"),
            br(),
            br()
            ),
      tabPanel("3g. Model details", tabsetPanel(
        tabPanel("3g-1. Model codes",
                p(tags$strong("Model codes for analysis of all studies")),
                downloadButton('download_code'),
                verbatimTextOutput("code")
                ),
        tabPanel("3g-2. Initial values",
                p(tags$strong("Initial values")),
                downloadButton('download_inits_1', "Download initial values for chain 1"),
                downloadButton('download_inits_2', "Download initial values for chain 2"),
                downloadButton('download_inits_3', "Download initial values for chain 3"),
                downloadButton('download_inits_4', "Download initial values for chain 4"),
                verbatimTextOutput("inits")
                ),
        tabPanel("3g-3. Download simulations",
                 p(tags$strong("Download simulated data")),
                 downloadButton('download_data1', "Download data from chain 1"),
                 br(),
                 downloadButton('download_data2', "Download data from chain 2"),
                 br(),
                 downloadButton('download_data3', "Download data from chain 3"),
                 br(),
                 downloadButton('download_data4', "Download data from chain 4")
        ),
        tabPanel("3g-4. Deviance details",
        fluidRow(
         column(6,
                p(tags$strong("Deviance data for all studies")),
                verbatimTextOutput("dev")),
         column(6,
                p(tags$strong("Deviance data for sensitivity analysis")),
               verbatimTextOutput("dev_sub")
         )),
        fluidRow(
               column(6,verbatimTextOutput("dev_ume")),
               column(6,verbatimTextOutput("dev_ume_sub")
               ))   
))))))))
),


###################################
### Tab 4 - User Guide ###
###################################

tabPanel(id= "guide","User Guide",
  
         fluidRow(
           column(1),
           column(10,
          h2 (tags$strong("User Guide")),
          h4 (tags$strong("A complete User Guide for MetaInsight regular version", style = "cursive;
                      color: #2196c4")),
          p("Please click the button below to download a copy of the MetaInsight User Guide. The specific User Guide for Metainsight: COVID 19 will come soon."),
          downloadButton("UG", "Download User Guide"),
          br(),
          br(),
          h4 (tags$strong("Cochrane training Webinar materials", style = "cursive;
                      color: #2196c4")),
           p(tags$strong("MetaInsight: Background, introduction, demonstration, limitations, and future plans")),
         p("These videos were recorded live as part of the ", 
           tags$a(href="https://training.cochrane.org/resource/metainsight-complex-review-support-unit-crsu-network-meta-analysis-nma-web-based-app", "Cochrane Training network meta-analysis learning live webinar series.",target="_blank"), 
           "They are intended for people who are interested in undertaking a network meta-analysis using MetaInsight."),
         br(),
         fluidRow(column(5,
                   HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/RR_tkICQv_s" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
         br(),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/b-fYoUdksRo" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
         br(),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/g0n5yxQ4Z34" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
         
                  ),
         column(1),
         column(4,
                h4(icon("hand-point-right"), tags$strong("Interested in using MetaInsight to analyse your own data and explore its full functionality?")),
                  br(),
                  br(),
                  h4(tags$strong(" Please go to our regular version of the app:"), 
                     tags$a(href="https://crsu.shinyapps.io/metainsight", tags$strong("https://crsu.shinyapps.io/metainsight"),target="_blank"), tags$strong(".")),
                  br(),
                  br(),
                  img(src='loaddata.jpg', width=700, height=450)
      
                )),
         br(),
         br()
         )
         )
         ),


###################################
### Tab 5 - Known issues ###
###################################
tabPanel(id= "guide","Known issues",
         
         fluidRow(
           column(1),
           column(10,
                  h2(tags$strong("Known issues")),
                  br(),
                  br(),
                  p (tags$strong("There are several known issues of the app:", style = "cursive;
                      color: #2196c4")),
                  p(icon("angle-double-right"), "The app has been tested using the Chrome web browser. If there are display issues 
                    in other browsers, you can consider using Chrome"),
                  p(icon("angle-double-right"), "Some users may notice that there is a displaying problem of the forest plot results on the side by side layout on the detailed analysis page. 
                    This is caused by the long treatment names. We are currently trying to resolve this."),
                  p(icon("angle-double-right"), "-	For two-arm studies, if both arms had 0 events, the frequentist analysis will automatically remove the whole study. For multi-arm studies, 
                    the arms that had 0 event will be removed and if more than one arms remain, the analysis will proceed, otherwise, the analysis 
                    will not be executed due to only a single arm remaining for one or more study (these will need removing manually). This 
                    issue is not present in Bayesian analysis; all studies will be included in the Bayesian analysis, regardless of the outcome. 
                    Note: the two alternative network plots that can be selected come from a frequentist and a Bayesian package so they can display 
                    different numbers of trials when one or more of the trials have 0 events for a particular outcome. "),
                  p("If the result is not shown or the app disconnects, it can be due to the following reasons:"),
                  p("   a. The network is disconnected. This can be checked on the '1c. Network plot' page on the 'Detailed data analysis' page."),
                  p("   b. Not all the arms of one or more study are selected into the analysis. Please check the data table titled 'View the included studies after filtering or individual selection' to check the selection."),
                  p("   c. Only one study remains after excluding studies on the sidebar of 'Select studies to excluded' on the 'Detailed data analysis' page."),
                  p("   d. There is an automatic time-out disconnection after a period inactivity"),
                  p("If the app frequently disconnects not due to any of the above reasons, please contact the developers.")


           )
         )
)    




)
)
