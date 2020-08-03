header <- dashboardHeader(title = "MND-SOLES-CT")

dashboardSidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Drug recommendation",
    tabName = "liveDR",
    icon = icon("clipboard-list")),
  
  menuItem(
    "Heatmap",
    tabName="heatmap",
    icon= icon("search")),
  
  menuItem(
    "Progress",
    tabName = "progress",
    icon = icon("pie-chart"),
    menuSubItem("Clinical review", tabName = "clinical"),
    menuSubItem("Animal In vivo review", tabName = "invivo"),
    menuSubItem("In vitro review", tabName = "invitro")
  ),
  
  menuItem("Drug CVs", 
           tabName = "drugCV", 
           icon = icon("tablets")),
  
  menuItem(
    "Quick downloads",
    tabName = "download",
    icon = icon("download")
  ),
  
  menuItem("About", tabName = "about", icon = icon("info"))
  ))

body <- dashboardBody(
  h2("DISCLAIMER: THIS IS A DEMO VERSION"),
  tabItems(
    
    # liveDR---------
    tabItem(tabName = "liveDR",
            fluidRow(
              box(
                h1("MND-SOLES CT"),
                h3("Motor Neuron Disease Systematic Online Living Evidence Summary for Clinical Trials"),
                p(strong("MND-SOLES-CT"), "aims to provide a living summary of evidence to guide prioritisation of drugs for evaluation in MND clinical trials. MND-SOLES CT reports current curated evidence from the"  ,strong("Repurposing Living Systematic Review-MND (ReLiSyR-MND)"), ", a three-part machine learning assisted living systematic review of:"),
                p("1. Clinical literature of MND and other neurodegenerative diseases which may share common pivotal pathways, namely, Alzheimer's disease (AD), Frontotemporal dementia (FTD), Huntington's disease (HD), Multiple Sclerosis (MS) and Parkinson's disease (PD). We scored each publication based on efficacy, safety, quality and study size against a predefined metric and generated a product score for each drug."),
                p("2. Animal in vivo literature of MND and FTD models. We performed a meta-analysis and calculated the standardised mean difference (SMD) for outcomes reported, with survival being our primary outcome of interest."),
                p("3. In vitro studies of MND and FTD models including induced pluripotent stem cell studies. We performed a meta-analysis and calculated the standardised mean difference (SMD) for outcomes reported, with cell death being our primary outcome of interest."),
                p("On this page we curate a current overview of the evidence from these three reviews."),
                p("More information on our methodology can be found under the About tab.")
                , width=12)),
            
            fluidRow(
              box(title="Overview Chart",
                  plotlyOutput("livedrugrankchart"), "This bubble chart plots drugs in our review according to", strong("Clinical Product Score"), "(colour scale)", strong("number of clinical publications"), "represented by size of bubble,", strong("standard mean difference (SMD) of survival in in vivo studies"), "on the x-axis, and", strong("SMD of cell death in in vitro studies"), "on the y-axis.", width=12
              )
            ), 
            
            
            fluidRow(
              box(
                width = 12,
                DT::dataTableOutput("livedrugranklist"),
                downloadButton("downloadLiveDrugTable", label = "Download drug score table")
              ))),
    
    
    
    
    
    
    ####heatmap
    tabItem(tabName="heatmap", 
            fluidRow(
              box(
                plotlyOutput("hm"), height=1500, width=10
              )
            )),
    
    
    # clinical progress tab-------------------------------------
    
    tabItem(
      tabName = "clinical",
      h1("Clinical review"),
      h4("last updated:", clinicalUpdateDate),
      
      fluidPage(
        
        infoBoxOutput("ClinicalUniquePubs"),
        infoBoxOutput("ClinicalIncludedPubs"),
        #    infoBoxOutput("ClinicalDrugMeetLogic"),
        
        #   infoBoxOutput("ClinicalPublicationsMeetLogic"),
        #    infoBoxOutput("ClinicalCoreDrugs"),
        infoBoxOutput("ClinicalCorePubs"),
        
        infoBoxOutput("ClinicalSingleAnnot"),
        infoBoxOutput("ClinicalDualAnnot"),
        infoBoxOutput("ClinicalReconciled"),
        
        
        fluidRow(
          column(width=8,
                 
                 
                 box(title="Clinical studies overview", 
                     p("This interactive sunburst chart displays all annotated papers according to disease studied, study design type, study phase, and drug. Hover over each chunk to see how many records fit each category. Select a category to expand and view subcategories in more details. Click the centre of chart to return to the previous level. "),
                     plotlyOutput("sb2", height="100%", width="100%"), width=NULL)),
          
          column(width=4,
                 
                 
                 box(
                   title = "1. Living search", width=NULL,
                   solidHeader = TRUE,
                   status = "danger",
                   "From our automated living search of PubMed, we have identified a total number of",
                   nClinicalUniquePubs,
                   "unique publications as of",
                   clinicalUpdateDate,
                   "."
                 ),
                 
                 box(
                   title = "2. Citation screening", width=NULL,
                   solidHeader = TRUE,
                   status = "warning",
                   "Using a combination of human and machine learning citation screening via the",
                   tags$a(href = "https://app.syrf.org.uk/home", "Systematic Review Facility (SyRF) platform"),
                   "we identified",
                   nClinicalIncludedPubs,
                   "publications meeting our inclusion criteria on title and abstract screening."
                 ),
                 
                 box(
                   title = "3. Filtering drugs of interest based on inclusion logic",width=NULL,
                   solidHeader = TRUE,
                   status = "info",
                   "Using a combination of human and machine learning drug and disease annotation via the",
                   tags$a(href = "https://app.syrf.org.uk/home", "Systematic Review Facility (SyRF) platform"),
                   "we have identified",
                   nClinicalDrugMeetLogic,
                   "drugs described in",
                   nClinicalPublicationsMeetLogic,
                   "publications where the drugs have been studied in at least one study in MND or two or more other diseases of interest (Alzheimer's disease, Frontotemporal dementia, Huntington's disease, Multiple Sclerosis, Parkinson's disease) which may share similar pivotal pathways."
                 ),
                 
                 box(
                   title = "4. Longlisting by clinical trial investigators",width=NULL,
                   solidHeader = TRUE,
                   status = "primary",
                   "These drugs have been reviewed by our clinical trial investigators, who have longlisted",
                   nClinicalCoreDrugs,
                   "drugs for prioritisation of data extraction taking into account feasibility for repurposing in clinical trials in near future, previous clinical trials and biological plausibility."
                 ),
                 
                 box(
                   title = "5. Data extraction",width=NULL,
                   solidHeader = TRUE,
                   status = "success",
                   "Our team of reviewers are extracting data from these publications. Currently,",
                   nClinicalSingleAnnotated,
                   "(",
                   round(percentClinicalSingleAnnotated,2),
                   "%) of publications for longlisted drugs have been single annotated,",
                   nClinicalDualAnnotated,
                   "(",
                   round(percentClinicalDualAnnotated,2),
                   "%) have been dual annotated and",
                   nClinicalReconciled,
                   "(",
                   round(percentClinicalReconciled,2),
                   "%) have been fully reconciled."
                 )
          ))
        
        
      )),
    
    # invivo progress------------------------------------- 
    tabItem(tabName = "invivo",
            h1("In vivo review"),
            h4("last updated:", InvivoUpdateDate),
            
            fluidPage(
              
              infoBoxOutput("InvivoUniquePubs"),
              infoBoxOutput("InvivoIncludedPubs"),
              #        infoBoxOutput("InvivoDrugMeetLogic"),
              
              #        infoBoxOutput("InvivoPublicationsMeetLogic"),
              #       infoBoxOutput("InvivoCoreDrugs"),
              infoBoxOutput("InvivoCorePubs"),
              
              infoBoxOutput("InvivoSingleAnnot"),
              infoBoxOutput("InvivoDualAnnot"),
              infoBoxOutput("InvivoReconciled"),
              
              box(
                title = "1. Living search",
                solidHeader = TRUE,
                status = "danger",
                "From our automated living search of PubMed, we have identified a total number of",
                nInvivoUniquePubs,
                "unique in vivo publications as of",
                InvivoUpdateDate,
                "."
              ),
              
              box(
                title = "2. Citation screening",
                solidHeader = TRUE,
                status = "warning",
                "Using a combination of human and machine learning citation screening via the",
                tags$a(href = "https://app.syrf.org.uk/home", "Systematic Review Facility (SyRF) platform"),
                "we identified",
                nInvivoIncludedPubs,
                "in vivo publications meeting our inclusion criteria on title and abstract screening."
              ),
              
              box(
                title = "3. Filtering drugs of interest based on inclusion logic",
                solidHeader = TRUE,
                status = "info",
                "Using a combination of human and machine learning drug and disease annotation via the",
                tags$a(href = "https://app.syrf.org.uk/home", "Systematic Review Facility (SyRF) platform"),
                "we have identified",
                nInvivoDrugMeetLogic,
                "drugs described in",
                nInvivoPublicationsMeetLogic,
                "in vivo publications where the drugs have been studied in at least one clinical study in MND or two or more other diseases of interest (Alzheimer's disease, Frontotemporal dementia, Huntington's disease, Multiple Sclerosis, Parkinson's disease) which may share similar pivotal pathways."
              ),
              
              box(
                title = "4. Longlisting by clinical trial investigators",
                solidHeader = TRUE,
                status = "primary",
                "These drugs have been reviewed by our clinical trial investigators, who have longlisted",
                nClinicalCoreDrugs,
                "drugs for prioritisation of data extraction taking into account feasibility for repurposing in clinical trials in near future, previous clinical trials and biological plausibility. Filtering for longlisted drugs, we identified", 
                nInvivoCoreDrugs,
                "drugs described in",
                nInvivoCoreDrugsPubs,
                "in vivo publications."
              ),
              
              box(
                title = "5. Data extraction",
                solidHeader = TRUE,
                status = "success",
                "Our team of reviewers are extracting data from these publications. Currently,",
                nInvivoSingleAnnotated,
                "(",
                round(percentInvivoSingleAnnotated,2),
                "%) of in vivo publications for longlisted drugs have been single annotated,",
                nInvivoDualAnnotated,
                "(",
                round(percentInvivoDualAnnotated, 2),
                "%) have been dual annotated and",
                nInvivoReconciled,
                "(",
                round(percentInvivoReconciled, 2),
                "%) have been fully reconciled."
              )
            )
    ),
    
    
    # invitro progress-------------------------------------
    tabItem(tabName = "invitro",
            h1("In vitro review"),
            h4("last updated:", InvitroUpdateDate),
            
            fluidPage(
              
              infoBoxOutput("InvitroUniquePubs"),
              infoBoxOutput("InvitroIncludedPubs"),
              #     infoBoxOutput("InvitroDrugMeetLogic"),
              
              #    infoBoxOutput("InvitroPublicationsMeetLogic"),
              #    infoBoxOutput("InvitroCoreDrugs"),
              infoBoxOutput("InvitroCorePubs"),
              
              infoBoxOutput("InvitroSingleAnnot"),
              infoBoxOutput("InvitroDualAnnot"),
              infoBoxOutput("InvitroReconciled"),
              
              
              
              
              
              
              box(
                title = "1. Living search",
                solidHeader = TRUE,
                status = "danger",
                "From our automated living search of PubMed, we have identified a total number of",
                nInvitroUniquePubs,
                "unique in vitro publications as of",
                InvitroUpdateDate,
                "."
              ),
              
              box(
                title = "2. Citation screening",
                solidHeader = TRUE,
                status = "warning",
                "Using a combination of human and machine learning citation screening via the",
                tags$a(href = "https://app.syrf.org.uk/home", "Systematic Review Facility (SyRF) platform"),
                "we identified",
                nInvitroIncludedPubs,
                "in vitro publications meeting our inclusion criteria on title and abstract screening."
              ),
              
              box(
                title = "3. Filtering drugs of interest based on inclusion logic",
                solidHeader = TRUE,
                status = "info",
                "Using a combination of human and machine learning drug and disease annotation via the",
                tags$a(href = "https://app.syrf.org.uk/home", "Systematic Review Facility (SyRF) platform"),
                "we have identified",
                nInvitroDrugMeetLogic,
                "drugs described in",
                nInvitroPublicationsMeetLogic,
                "in vitro publications where the drugs have been studied in at least one clinical study in MND or two or more other diseases of interest (Alzheimer's disease, Frontotemporal dementia, Huntington's disease, Multiple Sclerosis, Parkinson's disease) which may share similar pivotal pathways."
              ),
              
              box(
                title = "4. Longlisting by clinical trial investigators",
                solidHeader = TRUE,
                status = "primary",
                "These drugs have been reviewed by our clinical trial investigators, who have longlisted",
                nClinicalCoreDrugs,
                "drugs for prioritisation of data extraction taking into account feasibility for repurposing in clinical trials in near future, previous clinical trials and biological plausibility. Filtering for longlisted drugs, we  identified", 
                nInvitroCoreDrugs,
                "drugs described in",
                nInvitroCoreDrugsPubs,
                "in vitro publications."
              ),
              
              box(
                title = "5. Data extraction",
                solidHeader = TRUE,
                status = "success",
                "Our team of reviewers are extracting data from these publications. Currently,",
                nInvitroSingleAnnotated,
                "(",
                round(percentInvitroSingleAnnotated,2),
                "%) of in vitro publications for longlisted drugs have been single annotated,",
                nInvitroDualAnnotated,
                "(",
                round(percentInvitroDualAnnotated, 2),
                "%) have been dual annotated and",
                nInvitroReconciled,
                "(",
                round(percentInvitroReconciled, 2),
                "%) have been fully reconciled."
              )
            )
    ), 
    
    # drugCVtab-------------------------------------
    tabItem(
      tabName="drugCV",
      h1("Drug CV"),
      fluidPage(
        selectInput("drug", "Select Drug", drugList, multiple=FALSE),
        tabsetPanel(type="tabs",
                    tabPanel("Overview",
                             h3("Overview Plot"),
                             fluidRow(
                               box(plotlyOutput("selecteddrugrankchart"),
                                   "This bubble chart plots drugs in our review according to Clinical Product Score (colour scale), number of clinical publications represented by size of bubble, standard mean difference (SMD) of survival in in vivo studies on the x-axis, and SMD of cell death in in vitro studies on the y-axis.",width=12)),
                             fluidRow(
                               box(
                                 width = 12,
                                 DT::dataTableOutput("selecteddrugranklist")
                               )
                             )),
                    tabPanel("Clinical",
                             h3("Clinical Summary"),
                             fluidRow(
                               column(width=4,
                                      box(width= NULL, height = 600,
                                          h4( "Score Summary"),
                                          DT::dataTableOutput("selectedclinscoresummary"))),
                               
                               #       h4( "Publication Summary"),
                               #     DT::dataTableOutput(("selectedclinpubsummary")
                               #     ))),
                               
                               column(width=4, 
                                      box(width= NULL, height=600,
                                          title= "Study details",
                                          
                                          plotlyOutput("sb3"))),
                               
                               column(width=4,
                                      box(width=NULL, height=600, 
                                          title = "Number of patients",
                                          plotlyOutput("ptsb")))
                             ),
                             
                             
                             
                             fluidRow(box(width=12,
                                          h4("Publications for selected drug"),
                                          DT::dataTableOutput("drugclinicalpublications"),
                                          downloadBttn("downloadDrugPublications", "Download publications for selected drug"))
                             )),
                    
                    tabPanel("in vivo",
                             h3("In vivo Summary"),
                             
                             
                             fluidRow(
                               column(width=3,
                                      box(width=NULL, title="Outcomes", height= 230,
                                          
                                          pickerInput("outcome", "Select outcomes", c("Survival", "Behavioural", "Biochemical", "Histological", "Others"), multiple=TRUE, selected= c("Survival", "Behavioural", "Biochemical", "Histological", "Others"),
                                                      options= pickerOptions(
                                                        actionsBox=T,
                                                        showContent=TRUE))), 
                                      
                                      box(width=NULL, title="Disease Models", height = 250,
                                          pickerInput("models", "Select model species", c("Mouse", "Rat", "Drosophila", "Yeast", "Zebrafish", "C.elegans", "Other"), multiple=TRUE, selected= c("Mouse", "Rat", "Drosophila", "Yeast", "Zebrafish", "C.elegans", "Other"),
                                                      options= pickerOptions(
                                                        actionsBox=T,
                                                        showContent=TRUE)), 
                                          checkboxInput("sod1", "Include SOD1 studies", value=TRUE))),
                               
                               column(width=9, 
                                      box(width=NULL, height=500, title="Forest Plot"))),
                             
                             fluidRow(
                               
                               box(width=12,
                                   h4("Publications for selected drug"),
                                   DT::dataTableOutput("druginvivopublications"),
                                   downloadBttn("downloadinvivoDrugPublications", "Download publications for selected drug"))
                             )),
                    
                    tabPanel("in vitro",
                             h3("In vitro Summary"),
                             fluidRow(
                               box(width=12, height = 500, h4("Forest Plot")),
                               box(width=12,
                                   h4("Publications for selected drug"),
                                   DT::dataTableOutput("druginvitropublications"),
                                   downloadBttn("downloadinvitroDrugPublications", "Download publications for selected drug"))
                             ))
                    
        ))
    ),
    #quickdownload tab
    
    
    
    # about tab-------------------------------------
    tabItem(
      tabName = "about",
      fluidRow(
        column(width=7,
               box(
                 title="What is MND-SOLES-CT?", width=NULL,
                 p(strong("MND-SOLES-CT"), "is a", 
                   tags$a(href="http://www.dcn.ed.ac.uk/camarades/", "CAMARADES"),
                   "SOLES (Systematic Online Living Evidence Summary) project aiming to provide a living summary of evidence to guide prioritisation of drugs for evaluation in MND clinical trials, specificially",
                   tags$a(href="https://mnd-smart.org.uk", "MND-SMART"),
                   ". MND-SOLES-CT reports evidence from", strong("Repurposing Living Systematic Review-MND (ReLiSyR-MND)"), "a three-part machine learning assisted living systematic reviews of:",
                   tags$ol(
                     tags$li(
                       "Clinical literature of MND and other neurodegenerative diseases which may share common pivotal pathways, namely, Alzheimer's disease (AD), Frontotemporal dementia (FTD), Huntington's disease (HD), Multiple Sclerosis (MS) and Parkinson's disease (PD)"),
                     tags$li("Animal in vivo literature of MND and FTD models."),
                     tags$li("In vitro studies of MND and FTD models including induced pluripotent stem cell studies.")
                   ))),
               
               box(title="Methodology", width=NULL,
                   p("Our methodology is detailed in our",
                     tags$a(href="TOUPDATEWHENAVAILABLE", "protocol."),
                     "We adopted a systematic approach of evaluating drug candidates which we have previously used to guide drug selection for the Multple Sclerosis Secondary Progreessive Multi-Arm Randomisation Trial (MS-SMART) a  multi-arm phase IIb randomised controlled trial comparing the efficacy of three neuroprotective drugs in secondary progressive multiple sclerosis. These principles of drug selection were published by",
                     tags$a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0117705","Vesterinen et al."),
                     "in 2015."),
                   p("This approach, which adopts a structured, systematic method combined with independent expert(s) evaluation, was designed to identify candidate drugs for evaluation in clinical trials for people with neurodegenerative diseases, including MND, on account of the compelling evidence for shared dysregulated pathways and processes across neurodegenerative disorders. 
                  Critically, the structured evaluation takes into account not only biological plausibility and efficacy but also safety and quality of previous studies. This includes adopting benchmark practice such as Delphi and PICOS framework."),
                   p("1.", strong("Living Search"), 
                     ": We use the",
                     tags$a(href="https://app.syrf.org.uk","Systematic Review Facility (SyRF) platform"),
                     ", taking as its starting point automatic updating of the PubMed search."),
                   p("2.", strong("Citation Screening"),
                     ": Using a machine learning algorithm which has been trained and validated using human decisions, publications are screened for inclusion based on title and abstract."), 
                   p("3.", strong("Filtering drugs by inclusion logic"),
                     ": Text mining approaches (Regular Expressions deployed in R and taking as source material title and abstract) are used to identify disease and drug studied. A second algorithm is used to identify drugs which have been tested in at least one clinical study in MND; or have been tested clinically in two of the other specified conditions."),
                   p("4.", strong("Longlisting by trial investigators"),
                     ": Trial investigators reviewed the drugs filtered, excluding drugs which met the following critera: (i) previously considered unsuitable by expert panel due to lack of biological plausibility, drugs with unfavourable safety profiles in MND patients and drugs tested more than 3 times in MND population; (ii) drugs available over-the-counter as these may affect trial integrity; (iii) compounds which are not feasible for the next arms due to supply issues, such as compopunds not listed in the current version of the British National Formulary; (iv) drugs without oral preparations; and (v) drugs that are deemed by investigators to be unsafe/inappropriate for clinical trial in the current setting."),
                   p("5.", strong("Data extraction"), 
                     ": Our team of reviewers extract data specified in our protocol on the",
                     tags$a(href="https://syrf.org.uk", "SyRF platform"),
                     "from all included publications for longlisted drugs. Each publication will be annotated by at least two reviewers, with any differences reconciled by a third reviewer."),
                   p("6.", strong("Data Analysis"),
                     ": We will analyse the results as follows:",
                     tags$ul(
                       tags$li("Clinical review: For each publication, we assigned scores (1-4) based on efficacy [E], safety [S], study size [SS] and quality [Q] according to our predefined metrc (see",
                               tags$a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0117705#sec006", "Vesterinen et al 2015"), 
                               ". For each drug, we calculated a clinical product score, which is the product of the median publication scores for safety, efficacy, quality, study size, and log10(1 + number of publications)."),
                       tags$li("Animal invivo review and in vitro review: An individual meta‐analysis will be carried out for each intervention identified. We will summarise the effects of interventions where there are 3 or more publications in which that intervention has been tested reporting findings from at least 5 experiments. Depending on the nature of the outcomes reported we will use either standardised mean difference (SMD) or normalised mean difference (NMD) random effects meta-analysis with REML estimates of tau. Specifically, if fewer than 70% of outcomes are suitable for NMD analysis we will use SMD. Differences between groups of studies will be identified using meta-regression."
                       ))
                   ))),
        
        
        
        
        
        
        
        column(width=4,
               box(title = "CAMARADES", width=NULL, status="danger", 
                   p("The", tags$a(href="http://www.dcn.ed.ac.uk/camarades/", "CAMARADES"), "(Collaborative Approach to Meta-Analysis and Review of 
                  Animal Data from Experimental Studies) group specialise in performing", strong("systematic review and meta-analysis"), "of data
                  from experimental studies. Our interests range from identifying potential sources of bias in in vivo and in vitro studies; 
                  developing automation tools for evidence synthesis; developing recommendations for improvements in the design and
                  reporting; through to developing meta-analysis methodology to better apply to in basic research studies."),
                   p("Follow us on twitter", tags$a(href="https://twitter.com/camarades_?", "@CAMARADES_"))),
               box(title="CAMARADES Evidence Summary Projects", width=NULL, status="danger",
                   p("CAMARADES have produced other projects providing curated online evidence summaries in other disease areas including the", 
                     tags$a(href="https://camarades.shinyapps.io/COVID-19-SOLES", "COVID-19 Systematic Online Living Evidence Summary (SOLES) project"), ",",
                     tags$a(href="https://camarades.shinyapps.io/LivingEvidence_AD", "Transgenic Animal Models of Alzheimer's Disease"), 
                     "and",
                     tags$a(href="https://khair.shinyapps.io/CIPN", "Chemotherapy induced peripheral neuropathy"), ".")),
               
               box(title = "MND-SMART", width=NULL, status="primary",
                   p("The MND-SOLES-CT is a collaboration with investigators of the", tags$a(href="https://mnd-smart.org","Motor Neurone Disease – Systematic Multi-arm Adaptive Randomised Trial (MND-SMART)"),
                     "team to inform selection of the drugs for future arms of the trial. MND-SMART is registered on clinicaltrials.gov",
                     tags$a(href="https://www.clinicaltrials.gov/ct2/show/NCT04302870", "(NCT04302870)"),
                     ". MND-SMART is an adaptive multi-arm multi-stage clinical trial aiming to efficiently evaluate repurposed drugs in MND. It is led by the",
                     tags$a (href="http://euanmacdonaldcentre.org/", "Euan MacDonald Centre"),
                     "based at the",
                     tags$a(href="https://www.ed.ac.uk/", "University of Edinburgh"),
                     "alongside colleagues from",
                     tags$a (href="https://www.ucl.ac.uk/", "University College London"),
                     "and the",
                     tags$a (href="https://warwick.ac.uk/", "University of Warwick"),
                     ". The trial receives funding from the",
                     tags$a( href="http://euanmacdonaldcentre.org/", "Euan MacDonald Centre for MND Research"),
                     ",",
                     tags$a(href="https://www.mndscotland.org.uk/", "MND Scotland"), 
                     "and",
                     tags$a(href="https://www.myname5doddie.co.uk/", "My Name'5 Doddie Foundation"),
                     "."
                   ))
               
               
        ))),
    
    tabItem(
      tabName="download",
      fluidRow(
        box(title="Download all fully categorised references", status="success", width=4,
            p("Download data from our dataset of fully categorised papers only (annotated by at least 2 reviewers)"),
            downloadBttn("catclinicalpubs", "Download categorised clinical references"),
            downloadBttn("catinvivopubs", "Download categorised in vivo references"),
            downloadBttn("catinvitropubs", "Download categorised in vitro references")),
        
        box(title= "Download all unique included references", status="primary", width=4,
            downloadBttn("allclinicalpubs",
                         label= "Download all clinical references"),
            downloadBttn("allinvivopubs",
                         label="Download all in vivo references"),
            downloadBttn("allinvitropubs", "Download all in vitro references"))
        
        
        
      )
    )
    
    
    
    
    
    
    
    
    
  ))

shinyUI(dashboardPage(skin = "blue",
                      header,
                      dashboardSidebar,
                      body))

