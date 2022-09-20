#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# explicit call to each library rather than lapply(..., require, ...) per https://community.rstudio.com/t/no-data-tables-package/126493
library(data.table); library(magrittr); library(stringr); library(lubridate)
library(ggplot2); library(shiny)

# source("CDN_election_regional_PR.R")
# move these files to avoid relative Linux pathing issues https://stackoverflow.com/questions/36878256/shiny-app-deployment-error-cannot-change-working-directory
party_colours_wp<-fread("wikipedia_cdn_party_colours.csv", encoding="UTF-8")
actual_vs_pr_results<-fread("FPTP_vs_regional_PR_all_elections.csv", encoding="UTF-8")
mp_cols<-which(grepl("MPs", names(actual_vs_pr_results)))
results_all_methods<-apply(actual_vs_pr_results[,.SD,.SDcols=mp_cols], MARGIN=1, sum)
has_elected<-actual_vs_pr_results[which(results_all_methods > 0),]

# Party colours per Wikipedia Project page --------
# https://en.wikipedia.org/w/index.php?title=Wikipedia:WikiProject_Political_parties_and_politicians_in_Canada/list_of_parties&oldid=1078477083#Federal_parties
parties_represented<-data.table(Party=unique(has_elected$Party))
setkey(parties_represented, "Party"); setkey(party_colours_wp, "Long_name")
parties_represented %<>% merge(party_colours_wp, by.x="Party", by.y="Long_name",
                               all.x=T)
# deal with diacritics in 2 QC parties
semi_manual_replace<-function(party_re) {
  parties_represented[grepl(party_re, Party, ignore.case=T),
                      Hexadecimal_colour:=unlist(party_colours_wp[
                        grepl(party_re, Long_name, ignore.case=T),
                        Hexadecimal_colour])]
}
semi_manual_replace("Bloc Q"); semi_manual_replace("Ralliement C")
# Bloc populaire canadien same colour as BQ
parties_represented[grepl("Bloc popul", Party, ignore.case=T),
                    Hexadecimal_colour:=unlist(party_colours_wp[
                      grepl("Bloc Q", Long_name, ignore.case=T),
                      Hexadecimal_colour])]
parties_represented<-parties_represented[,.(Party, Hexadecimal_colour)]

setkey(has_elected, "Party")
has_elected %<>% merge(parties_represented, all.x=T)
setkeyv(has_elected, c("election_year", "Region", "Party"))

# Server logic ------------------------------------------------------------
# SYSTEM_CHOICES<-list(FPTP = "First-past-the-post (FPTP)",
#                      regional_PR = "Regional PR")
SYSTEM_CHOICES<-c("FPTP", "Regional PR")
spacefied_names<-names(actual_vs_pr_results) %>% str_replace_all("_", " ")
col_lookup<-lapply(SYSTEM_CHOICES, function(s) which(grepl(s, spacefied_names)))
names(col_lookup)<-SYSTEM_CHOICES

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Regional PR vs FPTP results"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="year",
                  label="General election of:",
                  choices=unique(actual_vs_pr_results$election_year),
                  selected=max(actual_vs_pr_results$election_year)),
      selectInput(inputId="region",
                  label="Region:",
                  choices=unique(actual_vs_pr_results$Region)),
      selectInput(inputId="system",
                  label="Electoral system:",
                  choices=SYSTEM_CHOICES)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("bar_graph")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$bar_graph <- renderPlot({
    # print(paste(paste("ENTERING WITH", input$system), input$region, input$year, sep="; "))
    results_dt<-has_elected[Region==input$region &
                              election_year == input$year,]
    # print(results_dt)
    results_df<-as.data.frame(results_dt) # small, OK
    MPs_vector<-results_df[,col_lookup[[input$system]]]
    total_MPs<-sum(MPs_vector)
    # print(MPs_vector)
    results_df$MPs<-MPs_vector
    if(input$region=="NATIONAL")
      results_df$text<-paste(results_df$MPs,
                             paste0("(",
                                    round(100 * results_df$MPs / total_MPs, 1),
                                    "%)"))
    else
      results_df$text<-results_df$MPs
    # print(results_df)
    FONT_ADJUST<-1
    p<-results_df %>% ggplot(aes(x = Party, y = MPs, fill=Party)) +
      geom_bar(stat="identity", width=0.5) +
      scale_fill_manual(values = results_df$Hexadecimal_colour) +
      guides(fill="none") +
      geom_text(aes(y=floor(MPs / 2) + floor(0.02 * sum(MPs_vector)), # + floor(max(1, 0.02 * sum(MPs_vector))
                    label=text), size=4)
    # geom_text(aes(y = -2, label = paste0("(", MPs, ")")), 
    #           hjust = 1) + #, size = 11 * FONT_ADJUST / .pt) +
    # geom_text(aes(y = -100, label = Party), 
    #           hjust = 0) + #, size = 11 * FONT_ADJUST / .pt) +
    # scale_y_continuous(labels = NULL, limits = c(-100, 100))
    # geom_col(aes(fill = Hexadecimal_colour)) #aes(fill = supp), width = 0.7)
    p + coord_flip(clip = "off") + ggtitle(paste(total_MPs, "seats total")) # + theme(axis.text.y = element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
