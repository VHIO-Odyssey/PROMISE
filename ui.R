# Load packages
if(!require(shiny)) install.packages("shiny")
library(shiny)
if(!require(shinythemes)) install.packages("shinythemes")
library(shinythemes)
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(readr)) install.packages("readr")
library(readr)
if(!require(shinyWidgets)) install.packages("shinyWidgets")
library(shinyWidgets)
if(!require(htmltools)) install.packages("htmltools")
library(htmltools)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(powerSurvEpi)) install.packages("powerSurvEpi")
library(powerSurvEpi)
if(!require(plotly)) install.packages("plotly")
library(plotly)
if(!require(tableHTML)) install.packages("tableHTML")
library(tableHTML)
if(!require(rms)) install.packages("rms")
library(rms)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  htmlTemplate("index.html",
               radio_ecog = radioButtons("ecog", "", choices = c("0-1", "2+"), inline = TRUE),
               slider_pmn = sliderInput("pmn", "", min = 0, max = 55, value = 32, step = 0.1),
               radio_oncotreat = radioButtons("oncotreat", "", choices = c("PR/CR", "SD", "PD"), inline = TRUE),
               slider_ldh = sliderInput("ldh", "", min = 100, max = 1200, value = 365),
               slider_album = sliderInput("album", "", min = 2, max = 5, value = 3, step = 0.1),
               text_output_alive = textOutput(outputId = "alive"),
               text_output_group = textOutput(outputId = "group"),
               plot = plotlyOutput("plotly"),
               text_output_prisk = textOutput(outputId = "prisk")
  )
)
