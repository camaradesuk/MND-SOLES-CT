shinyServer(function(input, output, session) {
  
  showModal(modalDialog(
    title="Disclaimer: This web application is under development and has not been officially released yet."
  ))
  
  ##########LIVE VERSION#######################################
  #drug recommendations-------------------------------------
  
  #live drug recommendations-------------------------------------
  livedrugranklist <- reactive({
    mydf <- drugSummary %>%
      select(
        "Drug",
        "productScore",
        "nPublication",
        "efficacyScore"  ,
        "safetyScore"  ,
        "studySizeScore" ,
        "qualityScore"  ,
        "invivoSurvivalSMD",
        "nInvivoStudies"   ,
        "nCellDeathSMD"   ,
        "nInVitroStudies"
      ) %>%
      arrange(desc(productScore))
    
    cols <- c(    "productScore",
                  "efficacyScore"  ,
                  "safetyScore"  ,
                  "studySizeScore" ,
                  "qualityScore"    )
    
    mydf[,cols] <-round(mydf[,cols],2)
    return(mydf)
  })
  ########drugtable-------------------------------------
  output$livedrugranklist <- DT::renderDataTable(DT::datatable(
    livedrugranklist(),
    colnames=c("Drug",
               "Clinical Product Score",
               "Clinical n(Pub)", 
               "[E]",
               "[S]",
               "[SS]",
               "[Q]",
               "In vivo survival SMD",
               "in vivo n(Pub)",
               "In vitro cell death SMD",
               "in vitro n(Pub)"),
    filter="top",
    caption= htmltools::tags$caption(
      style='caption-side:bottom; text-align:center;', 
      "Current results summary of (i) clinical review: publications for interventions were scored against predefined criteria (see the About tab for more information). Clinical publications were assigned scores for efficacy[E], safety[S], study size [SS] and quality [Q] (1: worst, 4: best). For each intervention, using the median scores from publications, a product score was generated. (Product score = log10(1 + number of clinical publications)*[E]*[S]*[SS]*[Q]); (ii)in vivo and (iii) in vitro review are reported here by the standard mean deviation (SMD) in survival and cell death studies respectively calculated from our meta-analysis. n(Pub) refers to number of publications annotated for each drug in our review.")
  ))
  #######download drug table-------------------------------------
  output$downloadLiveDrugTable <- downloadHandler(
    filename = function() {
      file<-paste("drugtable-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(livedrugranklist(), 
                file=file,
                quote=T,
                row.names=F,
                na="")
    }
  )
  
  
  
  ##### enable when invivo/invitro data available.
  ####### LIVE drugbubblechart-------------------------------------
  # output$livedrugrankchart <- renderPlotly({
  #    livedrugrankchart <- plot_ly(
  #      NewDrugSummary,
  #      x = ~ invivoSurvivalSMD,
  #      y = ~ nCellDeathSMD,
  #      hoverinfo = 'text',
  
  #      text = ~ paste(
  #        '</br> Drug:',
  #        Drug,
  #        '</br> Clinical Score:',
  #        round(productScore, digits = 2),
  #        '</br> n(clinical publications):',
  #        nPublication,
  #        '</br> in vivo SMD:',
  #        round(invivoSurvivalSMD, digits = 2),
  #        '</br> in vitro SMD:',
  #        round(nCellDeathSMD),
  #        digits = 2
  #      ),
  #      type = 'scatter',
  #      mode = 'markers',
  #      
  #      marker = list(size = ~ nPublication,
  #                    opacity = 0.5,
  #                    color = ~ productScore,
  #                    colorscale = 'Viridis', 
  #                    reversescale=TRUE,
  #                    showscale=T,
  #                    colorbar=list(title='Product Score'),
  #                    sizemin= 3
  #     )
  #    )%>%
  #      layout(title="Clinical, in vivo and in vitro scores by drug",
  #             xaxis=(list(title="In vivo Survival SMD")),
  #             yaxis=(list(title="In vitro cell death SMD")))
  #    livedrugrankchart
  #  })
  
  
  
  ###############
  #drug heatmap-------------------------------------
  productscoredata <- drugSummary %>%
    select("productScore")%>%
    arrange(desc(productScore))
  productscoredata <- as.matrix(productscoredata)
  rownames(productscoredata) <- drugSummary$Drug
  
  clinicaldata <- drugSummary %>%
    select("efficacyScore"  ,
           "safetyScore"  ,
           "studySizeScore" ,
           "qualityScore")
  clinicaldata <- as.matrix(clinicaldata)
  rownames(clinicaldata) <- drugSummary$Drug
  
  
  preclinicaldata <- drugSummary %>%
    select("invivoSurvivalSMD", "nCellDeathSMD")
  preclinicaldata <- as.matrix(preclinicaldata)
  rownames(preclinicaldata) <- drugSummary$Drug
  
  
  output$hm<-renderPlotly({
    fig3<-plot_ly(z=productscoredata, x=colnames(productscoredata), y=rownames(productscoredata)
                  , type='heatmap', height=1300, width= 1000, colorscale="Viridis", reversescale=TRUE, 
                  colorbar=list(title='Clinical Product Score'))%>%
      layout(yaxis=list(autorange='reversed'),
             xaxis=(list(title="Clinical Product Score")))
    
    fig1<-plot_ly(z=clinicaldata, x=colnames(clinicaldata), y=rownames(clinicaldata)
                  , type='heatmap', colorscale="magma", colorbar=list(title='Clinical subscores'))
    
    fig2<-plot_ly(z=preclinicaldata, x=colnames(preclinicaldata), y=rownames(preclinicaldata)
                  , type='heatmap', colorscale= "Blues", reversescale=TRUE, colorbar=list(title='Preclinical SMD'))
    
    hm<-subplot(fig3,fig1,fig2, shareY=T, widths=c(1/7, 4/7, 2/7))%>%layout(height=1300, width=900)
    
  })
  
  
  #progress-------------------------------------
  
  #####plotlysunburst-----------------------------
  
  
  sunburstdata <- publicationList %>%
    select(Disease, studyType, phase, Drug, StudyIdStr)%>%
    unique() %>%
    group_by(Disease, studyType, phase, Drug, StudyIdStr) %>%
    count() %>%
    rename(value = n) %>%
    ungroup()
  
  DF0 <- sunburstdata %>% 
    group_by(Disease) %>% 
    unique() %>%
    summarise(value=sum(value))
  
  DF1 <- sunburstdata %>% 
    group_by(Disease, studyType) %>% 
    summarise(value=sum(value))
  
  DF2 <- sunburstdata %>% 
    group_by(Disease, studyType, phase) %>%
    summarise(value=sum(value))
  
  DF3 <- sunburstdata %>% 
    group_by(Disease, studyType, phase, Drug) %>%
    summarise(value=sum(value))
  
  df0 <- data.frame(
    ids = paste(DF0$Disease),
    labels = DF0$Disease,
    parents = "",
    values = DF0$value,
    stringsAsFactors = F
  )
  
  df1 <- data.frame(
    ids = paste(DF1$Disease, "-", DF1$studyType),
    labels = DF1$studyType,
    parents = paste(DF1$Disease),
    values = DF1$value,
    stringsAsFactors = F
  )
  
  df2 <- data.frame(
    ids = paste(DF2$Disease, "-", DF2$studyType, "-", DF2$phase),
    labels = DF2$phase,
    parents = paste(DF2$Disease, "-", DF2$studyType),
    values = DF2$value,
    stringsAsFactors = F
  )
  
  df3 <- data.frame(
    ids = paste(DF3$Disease, "-", DF3$studyType, "-", DF3$phase, "-", DF3$Drug),
    labels = DF3$Drug,
    parents = paste(DF3$Disease, "-", DF3$studyType, "-", DF3$phase),
    values = DF3$value,
    stringsAsFactors = F
  )
  
  df <- rbind(df0, df1, df2, df3)
  
  
  
  output$sb2 <- renderPlotly({
    
    p <- plot_ly(df,
                 ids = ~ids,
                 labels = ~labels,
                 parents = ~parents,
                 type = 'sunburst',
                 values =  ~values,
                 branchvalues = "total", 
                 insidetextorientation='auto',
                 insidetextfont = list(size=12),
                 marker = list(colors = "Viridis", 
                               line = list(color = "white", width=2)),
                 source = "sunburstPlot")%>%
      layout(autosize = F, 
             paper_bgcolor="transparent",
             plot_bgcolor='transparent',
             height=700,
             width=700,
             margin = list(b = 80, l = 80, r = 80, t = 100, pad = 0, autoexpand = TRUE))
    
    event_register(p, 'plotly_click')
    
    p
    
  })
  
  
  
  
  
  
  
  ########clinicalprogress-------------------------------------
  output$ClinicalUniquePubs <- renderInfoBox({
    infoBox(
      "Unique publications identified",
      as.numeric(nClinicalUniquePubs),
      icon = icon("file-text-o"),
      color = "red"
    )
  })
  
  output$ClinicalIncludedPubs <- renderInfoBox({
    infoBox(
      "Included publications",
      as.numeric(nClinicalIncludedPubs),
      icon = icon("file-text-o"),
      color = "orange"
    )
  })
  
  output$ClinicalDrugMeetLogic <- renderInfoBox({
    infoBox(
      "Drugs meeting inclusion logic",
      as.numeric(nClinicalDrugMeetLogic),
      icon = icon("tablets"),
      color = "aqua"
    )
  })
  
  output$ClinicalPublicationsMeetLogic <- renderInfoBox({
    infoBox(
      "Publications for included drugs",
      as.numeric(nClinicalPublicationsMeetLogic),
      icon = icon("file-text-o"),
      color = "aqua"
    )
  })
  
  output$ClinicalCoreDrugs <- renderInfoBox({
    infoBox(
      "Longlisted Drugs",
      as.numeric(nClinicalCoreDrugs),
      icon = icon("tablets"),
      color = "blue"
    )
  })
  
  output$ClinicalCorePubs <- renderInfoBox({
    infoBox(
      "Publications for longlisted drugs",
      as.numeric(nClinicalCoreDrugsPubs),
      icon = icon("file-text-o"),
      color = "blue"
    )
  })
  
  output$ClinicalSingleAnnot <- renderInfoBox({
    infoBox(
      "Single Annotated Publications",
      as.numeric(nClinicalSingleAnnotated),
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$ClinicalDualAnnot <- renderInfoBox({
    infoBox(
      "Dual Annotated Publications",
      as.numeric(nClinicalDualAnnotated),
      icon = icon("user-friends"),
      color = "green"
    )
  })
  
  
  output$ClinicalReconciled <- renderInfoBox({
    infoBox(
      "Reconciled publications",
      as.numeric(nClinicalReconciled),
      icon = icon("check"),
      color = "olive"
    )
  })
  
  ######in vivo progress-------------------------------------
  output$InvivoUniquePubs <- renderInfoBox({
    infoBox(
      "Unique publications identified",
      as.numeric(nInvivoUniquePubs),
      icon = icon("file-text-o"),
      color = "red"
    )
  })
  
  output$InvivoIncludedPubs <- renderInfoBox({
    infoBox(
      "Included publications",
      as.numeric(nInvivoIncludedPubs),
      icon = icon("file-text-o"),
      color = "orange"
    )
  })
  
  output$InvivoDrugMeetLogic <- renderInfoBox({
    infoBox(
      "Drugs meeting inclusion logic",
      as.numeric(nInvivoDrugMeetLogic),
      icon = icon("tablets"),
      color = "aqua"
    )
  })
  
  output$InvivoPublicationsMeetLogic <- renderInfoBox({
    infoBox(
      "Publications for included drugs",
      as.numeric(nInvivoPublicationsMeetLogic),
      icon = icon("file-text-o"),
      color = "aqua"
    )
  })
  
  output$InvivoCoreDrugs <- renderInfoBox({
    infoBox(
      "Longlisted Drugs",
      as.numeric(nInvivoCoreDrugs),
      icon = icon("tablets"),
      color = "blue"
    )
  })
  
  output$InvivoCorePubs <- renderInfoBox({
    infoBox(
      "Publications for longlisted drugs",
      as.numeric(nInvivoCoreDrugsPubs),
      icon = icon("file-text-o"),
      color = "blue"
    )
  })
  
  output$InvivoSingleAnnot <- renderInfoBox({
    infoBox(
      "Single Annotated Publications",
      as.numeric(nInvivoSingleAnnotated),
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$InvivoDualAnnot <- renderInfoBox({
    infoBox(
      "Dual Annotated Publications",
      as.numeric(nInvivoDualAnnotated),
      icon = icon("user-friends"),
      color = "green"
    )
  })
  
  
  output$InvivoReconciled <- renderInfoBox({
    infoBox(
      "Reconciled publications",
      as.numeric(nInvivoReconciled),
      icon = icon("check"),
      color = "olive"
    )
  })
  
  #######invitro progress-------------------------------------
  
  output$InvitroUniquePubs <- renderInfoBox({
    infoBox(
      "Unique publications identified",
      as.numeric(nInvitroUniquePubs),
      icon = icon("file-text-o"),
      color = "red"
    )
  })
  
  output$InvitroIncludedPubs <- renderInfoBox({
    infoBox(
      "Included publications",
      as.numeric(nInvitroIncludedPubs),
      icon = icon("file-text-o"),
      color = "orange"
    )
  })
  
  output$InvitroDrugMeetLogic <- renderInfoBox({
    infoBox(
      "Drugs meeting inclusion logic",
      as.numeric(nInvitroDrugMeetLogic),
      icon = icon("tablets"),
      color = "aqua"
    )
  })
  
  output$InvitroPublicationsMeetLogic <- renderInfoBox({
    infoBox(
      "Publications for included drugs",
      as.numeric(nInvitroPublicationsMeetLogic),
      icon = icon("file-text-o"),
      color = "aqua"
    )
  })
  
  output$InvitroCoreDrugs <- renderInfoBox({
    infoBox(
      "Longlisted Drugs",
      as.numeric(nInvitroCoreDrugs),
      icon = icon("tablets"),
      color = "blue"
    )
  })
  
  output$InvitroCorePubs <- renderInfoBox({
    infoBox(
      "Publications for longlisted drugs",
      as.numeric(nInvitroCoreDrugsPubs),
      icon = icon("file-text-o"),
      color = "blue"
    )
  })
  
  output$InvitroSingleAnnot <- renderInfoBox({
    infoBox(
      "Single Annotated Publications",
      as.numeric(nInvitroSingleAnnotated),
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$InvitroDualAnnot <- renderInfoBox({
    infoBox(
      "Dual Annotated Publications",
      as.numeric(nInvitroDualAnnotated),
      icon = icon("user-friends"),
      color = "green"
    )
  })
  
  
  output$InvitroReconciled <- renderInfoBox({
    infoBox(
      "Reconciled publications",
      as.numeric(nInvitroReconciled),
      icon = icon("check"),
      color = "olive"
    )
  })
  
  
  #drugCV-------------------------------------
  
  ####selected drug chart activate when invivo invitro available
  # output$selecteddrugrankchart<-renderPlotly({
  #    drugrankchart <- plot_ly(
  #      drugSummary,
  #      x = ~ invivoSurvivalSMD,
  #      y = ~ nCellDeathSMD,
  #      hoverinfo = 'text',
  #      
  #      text = ~ paste(
  #        '</br> Drug:',
  #        Drug,
  #        '</br> Clinical Score:',
  #        round(productScore, digits = 2),
  #        '</br> n(clinical publications):',
  #        nPublication,
  #        '</br> in vivo SMD:',
  #        round(invivoSurvivalSMD, digits = 2),
  #        '</br> in vitro SMD:',
  #        round(nCellDeathSMD),
  #        digits = 2
  #      ),
  #      type = 'scatter',
  #      mode = 'markers',
  #      
  #      marker = list(size = ~ nPublication,
  #                    opacity = 0.5,
  #                    color = ~ productScore,
  #                    colorscale = 'Viridis', 
  #                    reversescale=TRUE,
  #                    showscale=T,
  #                    colorbar=list(title='Product Score'),
  #                    sizemin= 3
  #      )
  #    )%>%
  #      layout(title="Clinical, in vivo and in vitro scores by drug",
  #             xaxis=(list(title="In vivo Survival SMD")),
  #             yaxis=(list(title="In vitro cell death SMD")))
  #    drugrankchart
  #    
  #    drugbubble<-drugrankchart%>%add_markers()
  #    drugbubble<-drugbubble%>%layout(annotations=drugannot(input$drug), showlegend=F)
  #    drugbubble
  #  })
  ###########################

  selecteddrugranklist <- reactive({
    drugSummary %>%
      filter(Drug%in% input$drug)%>%
      select(
        "Drug",
        "productScore",
        "nPublication",
        "efficacyScore"  ,
        "safetyScore"  ,
        "studySizeScore" ,
        "qualityScore"  ,
        "invivoSurvivalSMD",
        "nInvivoStudies"   ,
        "nCellDeathSMD"   ,
        "nInVitroStudies"
      ) %>%
      arrange(desc(productScore))
    
  })
  ########drugtable-------------------------------------
  output$selecteddrugranklist <- DT::renderDataTable(DT::datatable(
    selecteddrugranklist(),
    colnames=c("Drug",
               "Clinical Product Score",
               "Clinical n(Pub)", 
               "[E]",
               "[S]",
               "[SS]",
               "[Q]",
               "In vivo survival SMD",
               "in vivo n(Pub)",
               "In vitro cell death SMD",
               "in vitro n(Pub)"),
    caption= htmltools::tags$caption(
      style='caption-side:bottom; text-align:center;', 
      "Current results summary of (i) clinical review: publications for interventions were scored against predefined criteria (see the About tab for more information). Clinical publications were assigned scores for efficacy[E], safety[S], study size [SS] and quality [Q] (1: worst, 4: best). For each intervention, using the median scores from publications, a product score was generated. (Product score = log10(1 + number of clinical publications)*[E]*[S]*[SS]*[Q]); (ii)in vivo and (iii) in vitro review are reported here by the standard mean deviation (SMD) in survival and cell death studies respectively calculated from our meta-analysis. n(Pub) refers to number of publications annotated for each drug in our review."),
    list(dom='t',
         ordering=F),
    rownames=F
  ))
  
  #####ClinicalPubSummary-------------------------------------
  
  selectedclinpubsummary <- reactive({
    drugpubdata <- drugSummary %>%
      filter(Drug %in% input$drug) %>%
      select(
        "nPublication",
        "nMND",
        "nAD",
        "nFTD",
        "nHD",
        "nMS",
        "nPD"
      )
    
    tdrugpubdata <- t(as.matrix(drugpubdata))
    return(tdrugpubdata)
  })
  
  output$selectedclinpubsummary <-
    DT::renderDataTable(DT::datatable(
      selectedclinpubsummary(),
      rownames=c("All diseases of interest", 
                 "MND", 
                 "AD",
                 "FTD",
                 "HD",
                 "MS",
                 "PD"),
      colnames="Number of publications",
      list(dom = 't',
           ordering=F)
      
    )%>%
      formatStyle(
        0,
        target = "row",
        fontWeight = styleEqual("All diseases of interest", "bold"),
        backgroundColor = styleEqual("All diseases of interest", "lightblue")
      )
    )
  
  #####ClinicalScoreSummary-------------------------------------
  selectedclinscoresummary <- reactive({
    drugscoredata <- drugSummary %>%
      filter(Drug %in% input$drug) %>%
      select(
        "productScore",
        "efficacyScore",
        "safetyScore",
        "studySizeScore",
        "qualityScore"
      )
    
    tdrugscoredata <- t(as.matrix(drugscoredata))
    return(tdrugscoredata)
  })
  
  output$selectedclinscoresummary <-
    DT::renderDataTable(DT::datatable(
      selectedclinscoresummary(),
      rownames=c("Product Score",
                 "Efficacy Score",
                 "Safety Score",
                 "Study Size Score",
                 "Quality Score"),
      colnames="Score",
      list(
        dom = 't',
        ordering=F
      ))%>%
        formatStyle(
          0,
          target = "row",
          fontWeight = styleEqual("Product Score", "bold"),
          backgroundColor = styleEqual("Product Score", "lightblue")
        )
    )
  
  
  ######drugsunburst-------------------------------------
  
  
  sdsunburstdata <- reactive({
    drugsbdata<-publicationList %>%
      filter(Drug %in% input$drug)%>%
      select(Disease, studyType, phase, StudyIdStr)%>%
      unique() %>%
      group_by(Disease, studyType, phase, StudyIdStr) %>%
      count() %>%
      rename(value = n) %>%
      ungroup()
    
    
    DF0 <- drugsbdata %>% 
      group_by(Disease) %>% 
      unique() %>%
      summarise(value=sum(value))
    
    DF1 <- drugsbdata %>% 
      group_by(Disease, studyType) %>% 
      summarise(value=sum(value))
    
    DF2 <- drugsbdata %>% 
      group_by(Disease, studyType, phase) %>%
      summarise(value=sum(value))
    
    df0 <- data.frame(
      ids = paste(DF0$Disease),
      labels = DF0$Disease,
      parents = "",
      values = DF0$value,
      stringsAsFactors = F
    )
    
    df1 <- data.frame(
      ids = paste(DF1$Disease, "-", DF1$studyType),
      labels = DF1$studyType,
      parents = paste(DF1$Disease),
      values = DF1$value,
      stringsAsFactors = F
    )
    
    df2 <- data.frame(
      ids = paste(DF2$Disease, "-", DF2$studyType, "-", DF2$phase),
      labels = DF2$phase,
      parents = paste(DF2$Disease, "-", DF2$studyType),
      values = DF2$value,
      stringsAsFactors = F
    )
    
    
    
    df <- rbind(df0, df1, df2)
    
    return(df)
  })
  
  
  
  output$sb3 <- renderPlotly({
    
    p <- plot_ly(sdsunburstdata(),
                 ids = ~ids,
                 labels = ~labels,
                 parents = ~parents,
                 type = 'sunburst',
                 values =  ~values,
                 branchvalues = "total", 
                 insidetextorientation='auto',
                 insidetextfont = list(size=12),
                 marker = list(colors = "Viridis", 
                               line = list(color = "white", width=2)),
                 source = "sunburstPlot")%>%
      layout(autosize = F, 
             paper_bgcolor="transparent",
             plot_bgcolor='transparent',
             height=300,
             width=300,
             margin = list(b = 0, l = 0, r = 0, t = 30, pad = 0, autoexpand = TRUE))
    
    event_register(p, 'plotly_click')
    
    p
    
  })
  
  #####drugptsunburst---------------------
  
  sdptsunburstdata <- reactive({
    drugptsbdata<-publicationList %>%
      filter(Drug %in% input$drug)%>%
      select(Disease, studyType, phase, nPatients)%>%
      unique() %>%
      ungroup()
    
    
    DF0 <- drugptsbdata %>% 
      group_by(Disease) %>% 
      unique() %>%
      summarise(nPatients=sum(nPatients))
    
    DF1 <- drugptsbdata %>% 
      group_by(Disease, studyType) %>% 
      summarise(nPatients=sum(nPatients))
    
    DF2 <- drugptsbdata %>% 
      group_by(Disease, studyType, phase) %>%
      summarise(nPatients=sum(nPatients))
    
    df0 <- data.frame(
      ids = paste(DF0$Disease),
      labels = DF0$Disease,
      parents = "",
      values = DF0$nPatients,
      stringsAsFactors = F
    )
    
    df1 <- data.frame(
      ids = paste(DF1$Disease, "-", DF1$studyType),
      labels = DF1$studyType,
      parents = paste(DF1$Disease),
      values = DF1$nPatients,
      stringsAsFactors = F
    )
    
    df2 <- data.frame(
      ids = paste(DF2$Disease, "-", DF2$studyType, "-", DF2$phase),
      labels = DF2$phase,
      parents = paste(DF2$Disease, "-", DF2$studyType),
      values = DF2$nPatients,
      stringsAsFactors = F
    )
    
    
    
    df <- rbind(df0, df1, df2)
    
    return(df)
  })
  
  
  
  output$ptsb <- renderPlotly({
    
    p <- plot_ly(sdptsunburstdata(),
                 ids = ~ids,
                 labels = ~labels,
                 parents = ~parents,
                 type = 'sunburst',
                 values =  ~values,
                 branchvalues = "total", 
                 insidetextorientation='auto',
                 insidetextfont = list(size=12),
                 marker = list(colors = "Viridis", 
                               line = list(color = "white", width=2)),
                 source = "sunburstPlot")%>%
      layout(autosize = F, 
             paper_bgcolor="transparent",
             plot_bgcolor='transparent',
             height=300,
             width=300,
             margin = list(b = 0, l = 0, r = 0, t = 30, pad = 0, autoexpand = TRUE))
    
    event_register(p, 'plotly_click')
    
    p
    
  })
  
  
  
  
  
  
  
  #####clinicalpubs-------------------------------------
  
  
  
  
  selecteddrugclinicalpubtable <- reactive({
    drugclinicalpubtable <- publicationList %>%
      filter(Drug %in% input$drug) %>%
      select(
        "Title",
        "Disease",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "studyType",
        "phase",
        "nPatients"
      )
    
    return(drugclinicalpubtable)
  })
  
  output$drugclinicalpublications <-
    DT::renderDataTable(DT::datatable(
      selecteddrugclinicalpubtable(),
      colnames=c("Title",
                 "Disease",
                 "Drug",
                 "Year",
                 "Author",
                 "Journal",
                 "DOI",
                 "Type of study",
                 "Study Phase",
                 "n(patients)"),
      filter="top"
    ))
  
  selecteddrugclinicalpublications <- reactive({
    drugclinicalpublications <- publicationList %>%
      filter(Drug %in% input$drug) %>%
      select(
        "Title",
        "Disease",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "studyType",
        "phase",
        "nPatients",
        "efficacyScore",
        "safetyScore",
        "studySizeScore",
        "qualityScore"
      )%>%unique()
    
    return(drugclinicalpublications)
  })
  
  
  
  #####downloadclinicalpubs-------------------------------------
  output$downloadDrugPublications <- downloadHandler(
    filename = function() {
      file <- paste("clinicalpublications", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        selecteddrugclinicalpublications(),
        file = file,
        quote = T,
        row.names = F,
        na = ""
      )
    }
  )
  
  
  ######invivodrugCV
  selecteddruginvivotable <- reactive({
    druginvivopublications <- invivoPublicationList %>%
      filter(Drug %in% input$drug) %>%
      select(
        "Title",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "Model",
        "SOD1",
        "mortalityOutcome",	
        "behavioralOutcome",	
        "biochemicalOutcome",	
        "histologicalOutcome")%>%
      as.data.frame()
    druginvivopublications$mortalityOutcome<-factor(ifelse(druginvivopublications$mortalityOutcome == 1,"Mortality", ""))
    druginvivopublications$behavioralOutcome<-factor(ifelse(druginvivopublications$behavioralOutcome == 1,"Behavioural",""))
    druginvivopublications$biochemicalOutcome<-factor(ifelse(druginvivopublications$biochemicalOutcome == 1,"Biochemical",""))
    druginvivopublications$histologicalOutcome<-factor(ifelse(druginvivopublications$histologicalOutcome == 1,"Histological",""))
    
    outcomeType<-paste(druginvivopublications$mortalityOutcome, "",
                       druginvivopublications$behavioralOutcome, "",
                       druginvivopublications$biochemicalOutcome, "",
                       druginvivopublications$histologicalOutcome)
    
    
    druginvivopubtable<-cbind(druginvivopublications[,1:8],outcomeType)
    
    return(druginvivopubtable)
  })
  
  
  
  output$druginvivopublications <-
    DT::renderDataTable(DT::datatable(
      selecteddruginvivotable(),
      colnames=c("Title",
                 "Disease",
                 "Year",
                 "Author",
                 "Journal",
                 "DOI",
                 "Model Species",
                 "SOD1 model",
                 "Type of Outcome"),
      filter="top"
    ))
  #####downloadinvivopubs-------------------------------
  
  selecteddruginvivopublications <- reactive({
    df <- invivoPublicationList %>%
      filter(Drug %in% input$drug) %>%
      select(
        "Title",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "Model",
        "SOD1",
        "mortalityOutcome",	
        "behavioralOutcome",	
        "biochemicalOutcome",	
        "histologicalOutcome")%>%
      as.data.frame()
    df$mortalityOutcome<-factor(ifelse(df$mortalityOutcome == 1,"Yes","No"))
    df$behavioralOutcome<-factor(ifelse(df$behavioralOutcome == 1,"Yes","No"))
    df$biochemicalOutcome<-factor(ifelse(df$biochemicalOutcome == 1,"Yes","No"))
    df$histologicalOutcome<-factor(ifelse(df$histologicalOutcome == 1,"Yes","No"))
    
    
    return(df)
  })
  
  output$downloadinvivoDrugPublications <- downloadHandler(
    filename = function() {
      file <- paste("invivopublications", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        selecteddruginvivopublications(),
        file = file,
        quote = T,
        row.names = F,
        na = ""
      )
    }
  )
  
  ######invitrodrugCV
  
  selecteddruginvitropublications <- reactive({
    druginvitropublications <- invitroPublicationList %>%
      filter(Drug %in% input$drug) %>%
      select(
        "Title",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "cdOutcome",	
        "otherOutcome")%>%
      as.data.frame()
    druginvitropublications$cdOutcome<-factor(ifelse(druginvitropublications$cdOutcome == 1,"Yes","No"))
    druginvitropublications$otherOutcome<-factor(ifelse(druginvitropublications$otherOutcome == 1,"Yes","No"))
    
    
    
    return(druginvitropublications)
  })
  
  
  
  output$druginvitropublications <-
    DT::renderDataTable(DT::datatable(
      selecteddruginvitropublications(),
      colnames=c("Title",
                 "Disease",
                 "Year",
                 "Author",
                 "Journal",
                 "DOI",
                 "Cell Death Outcome",
                 "Other Outcome"),
      filter = "top"
    ))
  #####downloadinvitropubs-------------------------------
  output$downloadinvitroDrugPublications <- downloadHandler(
    filename = function() {
      file <- paste("invitropublications", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        selecteddruginvitropublications(),
        file = file,
        quote = T,
        row.names = F,
        na = ""
      )
    }
  )
  
  
  
  
  
  #download---------------------------------------------
  #
  pList<-    as.data.frame(publicationList)
  
  output$catclinicalpubs<-downloadHandler(
    filename=function() {
      file<-paste("cat_clinicalpubs", Sys.Date(), ".csv", sep="")},
    content=function(file){
      write.csv(
        pList,
        file=file, 
        quote=T,
        row.names=F,
        na="")})
  
  output$catinvivopubs<-downloadHandler(
    filename=function(){
      file<-paste("cat_invivopubs", Sys.Date(), ".csv", sep="")},
    content=function(file){
      write.csv(
        invivoPublicationList(),
        file=file, 
        quote=T,
        row.names=F,
        na="")})
  
  output$catinvitropubs<-downloadHandler(
    filename=function(){
      file<-paste("cat_invitropubs", Sys.Date(), ".csv", sep="")},
    content=function(file){
      write.csv(
        invitroPublicationList(),
        file=file, 
        quote=T,
        row.names=F,
        na="")})
  
  
  #####download all publications-----------------------
  #output$allclinicalpubs
  #output$allinvivopubs
  #output$allinvitropubs
  
})