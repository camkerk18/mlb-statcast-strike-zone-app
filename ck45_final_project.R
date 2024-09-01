library(httr)
library(tidyverse)
library(shiny)
library(ggplot2)
library(ggpubr)
library(png)
library(scales)

####################################
## modify working directory to this directory
final <- read_csv("final.csv",
                  show_col_types=FALSE)
final_hist <- read_csv("final_hist.csv",
                       show_col_types=FALSE)
plate_width <- 17 + 2 * (9/pi)

####################################

ui <- fluidPage(
  titlePanel("Pitching Analysis: 2017 - 2022"),
  tabsetPanel(
    tabPanel("Strike Zone",
             sidebarLayout(
               sidebarPanel(
                 width=3,
                 selectInput(inputId = "A",label="Pitcher",choices=unique(final$pitcher_name)),
                 uiOutput(outputId="G"),
                 dateInput("B", "Start Date:", min="2017-04-02",max="2022-11-05",value = "2017-04-02", format = "mm/dd/yy"),
                 dateInput("C", "End Date:", min="2017-04-02",max="2022-11-05",value = "2017-11-01", format = "mm/dd/yy"),
                 selectInput(inputId="D",label="Pitch Type",choices=character(0)),
                 selectInput(inputId="E",label="Outcome",choices=character(0)),
                 actionButton(inputId ="Update", label="Update")
               ),
               mainPanel(
                 plotOutput(outputId="A",width="750px",height="750px")
               )
             )
    ),
    tabPanel("Outcome Graph",
             sidebarLayout(
               sidebarPanel(width=3,
                            uiOutput(outputId="E"),
                            selectInput(inputId="I",label="Pitch Type",choices=character(0)),
                            selectInput(inputId="H",label="Outcome",choices=character(0)),
                            actionButton(inputId ="Update2", label="Update")
               ),
               mainPanel(
                 plotOutput(outputId="B",width="1100px",height="500px")
               )
             )
    ),
    tabPanel("Project Description",
             mainPanel(
               uiOutput(outputId = "C",width="600px"),
             )
    )
  )
)

server <- function(input, output) {
  
  reduced = reactive({
    final %>%
      filter(pitcher_name==input$A) %>%
      filter(game_date >= input$B) %>%
      filter(game_date <= input$C)
  })
  
  reduced_hist = reactive({
    final_hist %>%
      filter(pitcher_name==input$A) %>%
      filter(game_date >= input$B) %>%
      filter(game_date <= input$C)
  })
  
  observeEvent(input$A, {
    updateSelectInput(inputId = "D",
                      choices=sort(unique(reduced()$pitch)))
    updateSelectInput(inputId = "E",
                      choices=c("All",unique(reduced() %>% filter(events!="no_event") %>% select(events) %>% distinct(.))))
    
    updateSelectInput(inputId = "I",
                      choices=sort(unique(reduced_hist()$pitch)))
    updateSelectInput(inputId = "H",
                      choices=c(unique(reduced() %>% filter(events!="no_event") %>% select(events) %>% distinct(.))))
    
    hand <- as.character(reduced()$p_throws[1])
    output$G <- renderUI({
      formatted_text <- HTML(paste("Dominant Hand: ",hand,"<br><br>"))
      return(formatted_text)
    })
  })
  
  reduced_pitch = eventReactive(input$Update, {
    if (input$E=="All") {
      reduced() %>%
        filter(pitch==input$D)
    } else {
      reduced() %>%
        filter(pitch==input$D) %>%
        filter(events==input$E)
    }
  })
  
  reduced_pitch_hist = eventReactive(input$Update2, {
    reduced_hist() %>%
      filter(pitch==input$I)
  })
  
  plot <- reactive({
    outcomes <- unique(reduced_pitch()$type)
    
    k_zone_plot <- ggplot(reduced_pitch(),
                          aes(x = plate_x, y = plate_z))+ 
      geom_rect(
        xmin = -(plate_width/2)/12, 
        xmax = (plate_width/2)/12, 
        ymin = 1.5, 
        ymax = 3.6, color = "darkgrey", alpha = 0, size=0.75
      ) + 
      coord_equal() + 
      scale_x_continuous(
        "Horizontal location (ft.)", 
        limits = c(-2, 2)
      ) + 
      scale_y_continuous(
        "Vertical location (ft.)", 
        limits = c(0, 5)
      )+
      geom_point(alpha=0.75, aes(color=type))+
      labs(color="Outcome", title="Strike Zone")+
      theme(legend.title = element_text(size = 12,family="serif"),
            axis.title = element_text(size=18),
            axis.title.x = element_text(margin=margin(t=20),family="serif"),
            axis.title.y = element_text(margin=margin(r=20),family="serif"),
            plot.title = element_text(size=30, family="Georgia",hjust=0.5),
            plot.margin = margin(1,0,0,1,unit="cm")) 
    return(k_zone_plot)
  })
  
  plot_hist = reactive({
    plot <- ggplot(data = reduced_pitch_hist(), aes(x = game_date, y = !!as.symbol(input$H))) +
      geom_point(alpha=0.60)+
      geom_smooth(color="cadetblue4")+
      scale_y_continuous(breaks = seq(0, 15, by = 1))+
      labs(title = "Statistic Frequency by Pitch Type",
           x = "Date",
           y = "Frequency")+
      theme(axis.title = element_text(size=18),
            axis.title.x = element_text(margin=margin(t=20),family="serif"),
            axis.title.y = element_text(margin=margin(r=20),family="serif",angle=0,vjust=0.5),
            plot.title = element_text(size=24, family="Georgia",hjust=0.5),
            plot.margin = margin(1,0,0,1,unit="cm")
      )
    return(plot)
  })
  
  observeEvent(input$Update, {
    output$A <- renderPlot({
      plot()
    })
  })
  
  observeEvent(input$Update2, {
    output$B <- renderPlot({
      plot_hist()
    })
  })
  
  output$C <- renderUI({
    formatted_text <- HTML(paste(
      
      "
<h3>Project Description</h3><br>
<p>
The first part of this shiny app is a strike zone visual that categorizes pitches based on outcome. 
The user may select a pitcher and a time frame bounded by a start date and end date. 
From there, the user may choose from filtered lists of pitch types and event outcomes that specifically occur when the selected player played. 
Upon clicking the <i>Update</i> button, the resulting visual will display the recorded location of where the pitch crossed home plate, with respect to the average strike zone.  
The points are furthter differentiated inside of the visual by color, with different colors representing strikes, balls, and hits, as per the legend. 
</p>
<p>
The second tab includes a frequency table of event outcomes over time, dependent on pitch type. 
The inputs for this visual are the same as for the strike zone, though the only specific input able to be modified on this tab are the pitch type and outcome type.  
Upon clicking the <i>Update</i> button specifically on this tab, the app produces a graphic with each data point representing the count of the selected outcome, achieved with the selected pitch, during each specific game in the time interval. 
The smooth line plotted within the graphic fits a smooth trend line to model the average frequency of the selected statistic over time. 
</p>
<p>
The data used for this application is statcast pitch-by-pitch data collected for the years 2017 through 2022. 
We further filtered this data to include only the 100 pitchers with the most pitches thrown between 2017 and 2022.
</p>
<br>
<p><b>Authors:</b> Cameron Kerkemeyer, Zach Larson</p>
<p><b>Data Source:</b> “Statcast Search.” Baseballsavant.Com, baseballsavant.mlb.com/statcast_search. Accessed 1 May 2024. </p>


"
      
    ))
    
    return(formatted_text)
  })
  
  output$E <- renderUI({
    pitcher <- input$A
    start <- input$B
    end <- input$C
    pitchtype <- input$D
    
    formatted_text <- HTML(paste("<h3>Pitcher:</h3>",
                                 pitcher,"<br>",
                                 "<h3>Start Date:</h3>",
                                 start,"<br>",
                                 "<h3>End Date:</h3>",
                                 end,"<br><br>"))
    
    return(formatted_text)
  })
  
}

app = shinyApp(ui = ui, server = server)
app

