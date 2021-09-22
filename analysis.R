install.packages('plyr', repos = "http://cran.us.r-project.org")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library("plyr")
library("ggplot2")

bioactivity_data <- read.csv("bioactivity_data_final.csv")

library(shiny)
ui <- fluidPage(
  titlePanel("Exploratory data analysis of a bioactivity dataset for coronavirus target molecules"),
  
  mainPanel(
    h3("Dataset head:"),
    tableOutput("head"),
    
    br(),
    
    h3("Dataset summary:"),
    verbatimTextOutput("summary"),
    
    br(),
    
    h3("Frequency of the two bioactivity classes:"),
    plotOutput("barplot_bioactivity"),
    
    br(),
    
    h3("Scatterplot of MW versus LogP:"),
    plotOutput("scatterplot_MW_LogP"),
    
    br(),
    
    h3("pIC50 by bioactivity - Boxplot and Mann-Whitney-Wilcoxon Test:"),
    plotOutput("boxplot_pIC50"),
    verbatimTextOutput("wilcox_pIC50"),
    
    br(),
    
    h3("MW by bioactivity- Boxplot and Mann-Whitney-Wilcoxon Test:"),
    plotOutput("boxplot_MW"),
    verbatimTextOutput("wilcox_MW"),
    
    br(),
    
    h3("LogP by bioactivity - Boxplot and Mann-Whitney-Wilcoxon Test:"),
    plotOutput("boxplot_LogP"),
    verbatimTextOutput("wilcox_LogP"),
    
    br(),
    
    h3("NumHDonors by bioactivity - Boxplot and Mann-Whitney-Wilcoxon Test:"),
    plotOutput("boxplot_NumHDonors"),
    verbatimTextOutput("wilcox_NumHDonors"),
    
    br(),
    
    h3("NumHAcceptors by bioactivity - Boxplot and Mann-Whitney-Wilcoxon Test:"),
    plotOutput("boxplot_NumHAcceptors"),
    verbatimTextOutput("wilcox_NumHAcceptors")
  )
)

server <- function(input, output, session) {
  output$summary <- renderPrint({
    summary(bioactivity_data)
  })
  
  output$head <- renderTable({
    head(bioactivity_data)
  })
  
  # Frequency plot of the 2 bioactivity classes
  output$barplot_bioactivity <- renderPlot({
    bioactivity_counts <-  count(bioactivity_data$bioactivity_class)
    ggplot(data=bioactivity_counts, aes(x=x, y=freq, fill=x)) +
      geom_bar(stat="identity")
  }, res = 96)
  
  # Scatter plot of MW versus LogP
  output$scatterplot_MW_LogP <- renderPlot({
    ggplot(bioactivity_data, aes(x=MW, y=LogP, col=bioactivity_class, size=pIC50, edge="black")) + 
      geom_point() + 
      scale_size_continuous(range = c(0.2,1))
  }, res = 96)
  
  # Box plot - pIC50 value
  output$boxplot_pIC50 <- renderPlot({
    ggplot(bioactivity_data, aes(x=bioactivity_class, y=pIC50, fill=bioactivity_class)) + 
      geom_boxplot()
  }, res = 96)
  
  # Box plot - MW
  output$boxplot_MW <- renderPlot({
    ggplot(bioactivity_data, aes(x=bioactivity_class, y=MW, fill=bioactivity_class)) + 
      geom_boxplot()
  }, res = 96)
  
  # Box plot - LogP
  output$boxplot_LogP <- renderPlot({
    ggplot(bioactivity_data, aes(x=bioactivity_class, y=LogP, fill=bioactivity_class)) + 
      geom_boxplot()
  }, res = 96)
  
  # Box plot - NumHDonors
  output$boxplot_NumHDonors <- renderPlot({
    ggplot(bioactivity_data, aes(x=bioactivity_class, y=NumHDonors, fill=bioactivity_class)) + 
      geom_boxplot()
  }, res = 96)
  
  # Box plot - NumHAcceptors
  output$boxplot_NumHAcceptors <- renderPlot({
    ggplot(bioactivity_data, aes(x=bioactivity_class, y=NumHAcceptors, fill=bioactivity_class)) + 
      geom_boxplot()
  }, res = 96)
  
  # Wilcox test - pIC50 for active vs. inactive
  output$wilcox_pIC50 <- renderPrint({
    wilcox.test(pIC50 ~ bioactivity_class, data = bioactivity_data)
  })
  
  # Wilcox test - MW for active vs. inactive
  output$wilcox_MW <- renderPrint({
    wilcox.test(MW ~ bioactivity_class, data = bioactivity_data)
  })
  
  # Wilcox test - LogP for active vs. inactive
  output$wilcox_LogP <- renderPrint({
    wilcox.test(LogP ~ bioactivity_class, data = bioactivity_data)
  })
  
  # Wilcox test - NumHDonors for active vs. inactive
  output$wilcox_NumHDonors <- renderPrint({
    wilcox.test(NumHDonors ~ bioactivity_class, data = bioactivity_data)
  })
  
  # Wilcox test - NumHAcceptors for active vs. inactive
  output$wilcox_NumHAcceptors <- renderPrint({
    wilcox.test(NumHAcceptors ~ bioactivity_class, data = bioactivity_data)
  })
  
}

shinyApp(ui, server)

