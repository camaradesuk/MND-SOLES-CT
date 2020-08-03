library(googlesheets4)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(viridis)
library(RColorBrewer)
library(data.table)
library(readr)
library(knitr)
library(rmarkdown)
library(metaviz)

source("drugtochart.R")
source('configure.R')

drugSummary <-
  googlesheets4::read_sheet(googleSheetId, sheet = drugSummaryName)

progressSummary <-
  googlesheets4::read_sheet(googleSheetId, sheet = progressSummaryName)
publicationList <-
  googlesheets4::read_sheet(googleSheetId, sheet = publicationListName)

#invivoPublicationList <-
#  googlesheets4::read_sheet(googleSheetId, sheet = invivoPublicationListName)
#invitroPublicationList <-
#  googlesheets4::read_sheet(googleSheetId, sheet = invitroPublicationListName)

clinicalReviewSummary <- progressSummary[1, ]
clinicalUpdateDate <- clinicalReviewSummary$Date

nClinicalUniquePubs <- clinicalReviewSummary$nUniquePublications

nClinicalIncludedPubs <-
  clinicalReviewSummary$nIncludedPublications

nClinicalDrugMeetLogic <- clinicalReviewSummary$nDrugMeetLogic

nClinicalPublicationsMeetLogic <-
  clinicalReviewSummary$nPublicationsMeetLogic

nClinicalCoreDrugs <- clinicalReviewSummary$nCoreDrugs

nClinicalCoreDrugsPubs <-
  clinicalReviewSummary$nCoreDrugPublications

nClinicalSingleAnnotated <- clinicalReviewSummary$nSingleAnnotated

percentClinicalSingleAnnotated <-
  nClinicalSingleAnnotated / nClinicalCoreDrugsPubs * 100

nClinicalDualAnnotated <- clinicalReviewSummary$nDualAnnotated

percentClinicalDualAnnotated <-
  nClinicalDualAnnotated / nClinicalCoreDrugsPubs * 100

nClinicalReconciled <- clinicalReviewSummary$nReconciled

percentClinicalReconciled <-
  nClinicalReconciled / nClinicalCoreDrugsPubs * 100
######

InvivoReviewSummary <- progressSummary[2, ]
InvivoUpdateDate <- InvivoReviewSummary$Date

nInvivoUniquePubs <- InvivoReviewSummary$nUniquePublications
nInvivoIncludedPubs <-
  InvivoReviewSummary$nIncludedPublications

nInvivoDrugMeetLogic <- InvivoReviewSummary$nDrugMeetLogic

nInvivoPublicationsMeetLogic <-
  InvivoReviewSummary$nPublicationsMeetLogic

nInvivoCoreDrugs <- InvivoReviewSummary$nCoreDrugs

nInvivoCoreDrugsPubs <-
  InvivoReviewSummary$nCoreDrugPublications

nInvivoSingleAnnotated <- InvivoReviewSummary$nSingleAnnotated

percentInvivoSingleAnnotated <-
  nInvivoSingleAnnotated / nInvivoCoreDrugsPubs * 100

nInvivoDualAnnotated <- InvivoReviewSummary$nDualAnnotated

percentInvivoDualAnnotated <-
  nInvivoDualAnnotated / nInvivoCoreDrugsPubs * 100

nInvivoReconciled <- InvivoReviewSummary$nReconciled

percentInvivoReconciled <-
  nInvivoReconciled / nInvivoCoreDrugsPubs * 100
#########

InvitroReviewSummary <- progressSummary[3, ]
InvitroUpdateDate <- InvitroReviewSummary$Date

nInvitroUniquePubs <- InvitroReviewSummary$nUniquePublications
nInvitroIncludedPubs <-
  InvitroReviewSummary$nIncludedPublications

nInvitroDrugMeetLogic <- InvitroReviewSummary$nDrugMeetLogic

nInvitroPublicationsMeetLogic <-
  InvitroReviewSummary$nPublicationsMeetLogic

nInvitroCoreDrugs <- InvitroReviewSummary$nCoreDrugs

nInvitroCoreDrugsPubs <-
  InvitroReviewSummary$nCoreDrugPublications

nInvitroSingleAnnotated <- InvitroReviewSummary$nSingleAnnotated

percentInvitroSingleAnnotated <-
  nInvitroSingleAnnotated / nInvitroCoreDrugsPubs * 100

nInvitroDualAnnotated <- InvitroReviewSummary$nDualAnnotated

percentInvitroDualAnnotated <-
  nInvitroDualAnnotated / nInvitroCoreDrugsPubs * 100

nInvitroReconciled <- InvitroReviewSummary$nReconciled

percentInvitroReconciled <-
  nInvitroReconciled / nInvitroCoreDrugsPubs * 100

drugList<-sort(publicationList$Drug)

#Progress invivo
invivoReviewSummary <- progressSummary[2, ]

invivoUpdateDate <- invivoReviewSummary$Date

#Progress in vitro
invitroUpdateDate <- progressSummary$Date[3]
