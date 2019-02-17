library(ggplot2)
library(tidyverse)
library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(tidyr)



boys <- read_excel("Top100_Popular_Baby_Names.xlsx",2)
girls <- read_excel("Top100_Popular_Baby_Names.xlsx",1)
boys <- boys[-c(1:6),]
girls<- girls[-c(1:6),]
names(boys)<- 1:196
names(girls)<- 1:196
year<- c(1954:2018)
rank<-c(1:10)

names_allboys<-vector()
for (i in 1:65){
  names_allboys<- append(names_allboys,boys[[i*3]])
}
names_allboys<-unique(names_allboys)
names_allboys <-names_allboys[!is.na(names_allboys)]
names_allboys<- data.frame(names_allboys)
names_allboys<-sort(names_allboys$names_allboys)

names_allgirls<-vector()
for (i in 1:65){
  names_allgirls<- append(names_allgirls,girls[[i*3]])
}
names_allgirls<-unique(names_allgirls)
names_allgirls <-names_allgirls[!is.na(names_allgirls)]
names_allgirls<- data.frame(names_allgirls)
names_allgirls<-sort(names_allgirls$names_allgirls)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  titlePanel('Shiny Assignement 1'),
  
  # Sidebar layout with a input and output definitions
  tabsetPanel(
    
    tabPanel('Top 10 Popular Girls By Year',
             sidebarLayout(
               
               # Inputs
               sidebarPanel(
                 
                 # Select variable for y-axis
                 selectInput(inputId = "girls_year",
                             label = "Choose a Year:",
                             choices = year)
               ),
               # Outputs
               mainPanel(
                 tableOutput(outputId = "girls_table"),
                 htmlOutput('girls1')
               )
             )

    ),
    tabPanel('Top 10 Popular Boys By Year',
             sidebarLayout(
               
               # Inputs
               sidebarPanel(
                 
                 # Select variable for y-axis
                 selectInput(inputId = "boys_year",
                             label = "Choose a Year:",
                             choices = year)
               ),
               # Outputs
               mainPanel(
                 tableOutput(outputId = "boys_table"),
                 htmlOutput('boys1')
               )
             )
    ),
    tabPanel('Popularity of Girl by Year',
    sidebarLayout(
      
      # Inputs
      sidebarPanel(
        
        # Select variable for y-axis
        selectInput(inputId = "girls_name",
                    label = "Choose Name of the Girl:",
                    choices = names_allgirls)
      ),
      # Outputs
      mainPanel(
        plotOutput(outputId = "popular_girl",height =500)
      )
    )
    ),
    tabPanel('Popularity of Boy by Year',
             sidebarLayout(
               
               # Inputs
               sidebarPanel(
                 
                 # Select variable for y-axis
                 selectInput(inputId = "boys_name",
                             label = "Choose Name of the Boy:",
                             choices = names_allboys)
               ),
               # Outputs
               mainPanel(
                 plotOutput(outputId = "popular_boy",height =500)
               )
             )
             
    )
)
)

# Define server function required to create the scatterplot
server <- function(input, output) {
    

  
  # Create scatterplot object the plotOutput function is expecting
  output$boys_table <- renderTable({
      
      c<-match(input$boys_year,year)
      c<-3*c
      d<-c+1
      
      final<-data.frame(Names = boys[[c]], Frequency = boys[[d]])
      final<-head(final,10)
      final<-data.frame(Rank = rank ,final)
      
      final
  },
      width = 500,
      align = 'c'
      )
  output$boys1 <- renderUI({
    
    HTML(paste("<p style = 'font-size:16px'> Table shows 
               <b>top 10</b> 
                names of 
               <b>Boys </b>
               in year : 
               <b style = 'font-size:20px'>",
               input$boys_year,
               "</b></p>"))
    
  })
  
  output$girls_table <- renderTable({
    
    c<-match("1957",year)
    c
    c<-3*c
    d<-c+1
    
    final<-data.frame(Names = girls[[c]], Frequency = girls[[d]])
    final<-head(final,10)
    final<-data.frame(Rank = rank ,final)
    
    final
    
  },
  width = 500,
  align = 'c')
  
  output$girls1 <- renderUI({
    
    HTML(paste("<p style = 'font-size:16px'> Table shows 
        <b>top 10</b> 
        names of 
        <b>Girls </b>
        in year : 
        <b style = 'font-size:20px'>",
        input$girls_year,"</b></p>"))
    
  })
  
  output$popular_boy <- renderPlot({
    
    l<-factor()
    m<-factor()
    for (i in 1:65){
      i<-i*3
      for (j in 1:100){
        if(boys[[j,i]] == input$boys_name)
        {
          l<-append(l,year[i/3])
          m<-append(m,boys[[j,i+1]])
        }
      }
    }
    year<-as.numeric(l)
    Popularity<-as.numeric(m)
    final<-data.frame(year,Popularity)
    final<-final[order(final$Popularity),]
    
    ggplot(data=final , aes(x= year, y= Popularity),cex.lab=1.5) +
      geom_point(size = 2,color = "blue")+ylab("Popularity (frequency)")+
      geom_line(stat = "identity",size = 1,color = 'red',linetype = "dotdash")+
      xlim(1954,2018)+ ggtitle("Popularity of Boys' names over the Years")+
      theme(plot.title = element_text(hjust = 0.5,size = 20),
            text = element_text(size = 15,color = 'blue'))
    
  })
  
  output$popular_girl <- renderPlot({
    
    l<-factor()
    m<-factor()
    for (i in 1:65){
      i<-i*3
      for (j in 1:100){
        if(girls[[j,i]] == input$girls_name)
        {
          l<-append(l,year[i/3])
          m<-append(m,girls[[j,i+1]])
        }
      }
    }
    year<-as.numeric(l)
    Popularity<-as.numeric(m)
    final<-data.frame(year,Popularity)
    final<-final[order(final$Popularity),]
    
    ggplot(data=final , aes(x= year, y= Popularity),cex.lab=1.5) +
      geom_point(size = 2,color = "blue")+ ylab("Popularity (frequency)")+
      geom_line(stat = "identity",size = 1,color = 'red',linetype = "dotdash")+
      xlim(1954,2018)+ ggtitle("Popularity of Girls' names over the Years")+
      theme(plot.title = element_text(hjust = 0.5,size = 20),
            text = element_text(size = 15,color = 'blue'))
    
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

