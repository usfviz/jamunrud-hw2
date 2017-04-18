dependencies <- c('shiny', 'ggplot2', 'dplyr', 'shinyjs', 'ggvis')
new.packages <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages, repos="http://cran.rstudio.com/")}

library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(ggvis)

wb <- read.csv('world_bank.csv')
wb$id <- 1:nrow(wb)
wb$Country.Label <- ""
wb$Hover.Label <- ""
wb$X.Label <- ""
wb$Hover.X.Fertility <- 0
wb$Hover.Y.Fertility <- 0
wb$Y.Label <- ""
wb$Hover.X.Life <- 0
wb$Hover.Y.Life <- 0
wb$Opacity <- 1


ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs
  mainPanel(
    div(style = "height:450px;",
        ggvisOutput("plot1")
    ),
    sliderInput('year', label='Year', min=1960, max=2014, sep='', width='900px',
                value=1960, step=1, round=TRUE, ticks=FALSE,
                animate=animationOptions(interval=400, loop=FALSE)),
    tags$script(HTML("
        $(document).ready(function() {setTimeout(function() {
                     supElement = document.getElementById('size').parentElement;
                     $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
                     }, 50);})
                     ")),
    width = 8
    ),
  sidebarPanel(
    span("Region"), br(),
    span(style = "background: #3366CC; margin-right: 3px; margin-left: 3px; width: 10px; height: 10px; display: inline-block;"),
    span(id="EastAsia", "East Asia & Pacific", style = "font-size: 13px; display: inline-block;"),
    br(),
    span(style = "background: #DC3912; margin-right: 3px; margin-left: 3px; width: 10px; height: 10px; display: inline-block;"),
    span(id="Europe", "Europe & Central Asia", style = "font-size: 13px; display: inline-block;"),
    br(),
    span(style = "background: #FF9900; margin-right: 3px; margin-left: 3px; width: 10px; height: 10px; display: inline-block;"),
    span(id="LatinAmerica", "Latin America & Caribbean", style = "font-size: 13px; display: inline-block;"),
    br(),
    span(style = "background: #109618; margin-right: 3px; margin-left: 3px; width: 10px; height: 10px; display: inline-block;"),
    span(id="MiddleEast", "Middle East & North Africa", style = "font-size: 13px; display: inline-block;"),
    br(),
    span(style = "background: #990099; margin-right: 3px; margin-left: 3px; width: 10px; height: 10px; display: inline-block;"),
    span(id="NorthAmerica", "North America", style = "font-size: 13px; display: inline-block;"),
    br(),
    span(style = "background: #0099C6; margin-right: 3px; margin-left: 3px; width: 10px; height: 10px; display: inline-block;"),
    span(id="SouthAsia", "South Asia", style = "font-size: 13px; display: inline-block;"),
    br(),
    span(style = "background: #DD4477; margin-right: 3px; margin-left: 3px; width: 10px; height: 10px; display: inline-block;"),
    span(id="Africa", "Sub-Saharan Africa", style = "font-size: 13px; display: inline-block;"),
    br(),

    span('Population', style = "display: inline-block; width:70px; vertical-align: middle; font-weight: bold;"),
    span(sliderInput('size', label=NULL, min=1, max=9, sep='',
                    value=5, step=1, ticks=FALSE),
        style = "display: inline-block; width: 100px; vertical-align: middle;"),
    div(id='Population', htmlOutput('PopHTML'), style= "font-size: 20px; font-weight: bold;"),
    width = 4,
    style = 'background: white'
  )
)

server <- function(input, output) {
  bubble_range <- reactive({c((2^(10 - input$size))*1000000, (2^(10 - input$size))*20000000)})
  values <- reactiveValues(update = TRUE)
  
  
  
  wb_plot <- reactive({ df <- remove_missing(wb[which(wb$Year == input$year),], vars=c('Fertility.Rate', 'Life.Expectancy'))
                        df$update <- values$update
                        df <- arrange(df, desc(Population))
                        df
  })
  
  change_opacity <- function(region, value){
    wb[which(wb$Region != region), 'Opacity'] <<- value
    values$update = ifelse(isolate(values$update) == TRUE, FALSE, TRUE)
  }
  
  onevent("mouseenter", "EastAsia", function(e){ change_opacity('East Asia & Pacific', .1) })
  onevent("mouseleave", "EastAsia", function(e){ change_opacity('East Asia & Pacific', 1) })
  onevent("mouseenter", "Europe", function(e){ change_opacity("Europe & Central Asia", .1) })
  onevent("mouseleave", "Europe", function(e){ change_opacity("Europe & Central Asia", 1) })
  onevent("mouseenter", "LatinAmerica", function(e){ change_opacity("Latin America & Caribbean", .1) })
  onevent("mouseleave", "LatinAmerica", function(e){ change_opacity("Latin America & Caribbean", 1) })
  onevent("mouseenter", "MiddleEast", function(e){ change_opacity("Middle East & North Africa", .1) })
  onevent("mouseleave", "MiddleEast", function(e){ change_opacity("Middle East & North Africa", 1) })
  onevent("mouseenter", "NorthAmerica", function(e){ change_opacity("North America", .1) })
  onevent("mouseleave", "NorthAmerica", function(e){ change_opacity("North America", 1) })
  onevent("mouseenter", "SouthAsia", function(e){ change_opacity("South Asia", .1) })
  onevent("mouseleave", "SouthAsia", function(e){ change_opacity("South Asia", 1) })
  onevent("mouseenter", "Africa", function(e){ change_opacity("Sub-Saharan Africa", .1) })
  onevent("mouseleave", "Africa", function(e){ change_opacity("Sub-Saharan Africa", 1) })
  
  click_func <- function(data, location, session){
    country <- wb[which(wb$id == data$id), 'Country.Name']
    wb[which(wb$Country.Name == country), 'Country.Label'] <<- ifelse(wb[which(wb$Country.Name == country), 'Country.Label'] == '',
                                                                     as.character(country), '')
    values$update = ifelse(isolate(values$update) == TRUE, FALSE, TRUE)
  }
  
  hover_on_func <- function(data, location, session){
    country <- wb[which(wb$id == data$id), 'Country.Name']
    wb[which(wb$Country.Name == country), 'Hover.Label'] <<- ifelse(wb[which(wb$Country.Name == country), 'Country.Label'] == '',
                                                                      as.character(country), '')
    wb[which(wb$Country.Name == country), 'X.Label'] <<- ifelse(wb[which(wb$Country.Name == country), 'X.Label'] == '',
                                                                    round(wb[which(wb$Country.Name == country), 'Life.Expectancy'],1), '')
    wb[which(wb$Country.Name == country), 'Y.Label'] <<- ifelse(wb[which(wb$Country.Name == country), 'Y.Label'] == '',
                                                                round(wb[which(wb$Country.Name == country), 'Fertility.Rate'], 2), '')
    wb[which(wb$Country.Name == country), 'Hover.X.Life'] <<- ifelse(wb[which(wb$Country.Name == country), 'Hover.X.Life'] == 0,
                                                                wb[which(wb$Country.Name == country), 'Life.Expectancy'], 0)
    wb[which(wb$Country.Name == country), 'Hover.Y.Life'] <<- ifelse(wb[which(wb$Country.Name == country), 'Hover.Y.Life'] == 0,
                                                                     .1, 0)
    wb[which(wb$Country.Name == country), 'Hover.X.Fertility'] <<- ifelse(wb[which(wb$Country.Name == country), 'Hover.X.Fertility'] == 0,
                                                                          6, 0)
    wb[which(wb$Country.Name == country), 'Hover.Y.Fertility'] <<- ifelse(wb[which(wb$Country.Name == country), 'Hover.Y.Fertility'] == 0,
                                                                  wb[which(wb$Country.Name == country), 'Fertility.Rate'], 0)
    
    pop <- wb[which(wb$id == data$id), 'Population']
    
    output$PopHTML <- renderText({
      paste0(prettyNum(pop, big.mark=","))
    })
    
    values$update = ifelse(isolate(values$update) == TRUE, FALSE, TRUE)
  }
  
  hover_off_func <- function(session){
    wb$Hover.Label <<- ""
    wb$X.Label <<- ""
    wb$Y.Label <<- ""
    wb$Hover.X.Life <<- 0
    wb$Hover.Y.Life <<- 0
    wb$Hover.X.Fertility <<- 0
    wb$Hover.Y.Fertility <<- 0
    output$PopHTML <- renderText({''})
    values$update = ifelse(isolate(values$update) == TRUE, FALSE, TRUE)
  }
  
  wb_plot %>% ggvis(~Life.Expectancy, ~Fertility.Rate, key := ~id) %>%
    layer_points(fill = ~Region, fillOpacity := ~Opacity, size = ~Population, stroke := 'black', strokeWidth := 1) %>%
    set_options(hover_duration = 500) %>% 
    scale_numeric("x", domain = c(10, 90), nice = FALSE) %>%
    scale_numeric("y", domain = c(.5, 9), nice = FALSE) %>%
    scale_numeric("size", domain = bubble_range) %>%
    add_axis("x", title = "Life Expectancy", title_offset = 50) %>%
    add_axis("y", title = "Fertility Rate", title_offset = 50) %>%
    scale_ordinal("fill", range=c('#3366CC','#DC3912','#FF9900','#109618','#990099','#0099C6','#DD4477')) %>% 
    set_options(width = "auto", height = "auto", resizable=FALSE) %>% 
    handle_click(click_func) %>% 
    handle_hover(hover_on_func, hover_off_func) %>%
    layer_text(x = ~Life.Expectancy + 1, y = ~Fertility.Rate + .1, text := ~Country.Label, fontWeight := 'bold',
               fontSize := 20, fill = ~Region, stroke := 'black') %>%
    layer_text(x = ~Life.Expectancy + 1, y = ~Fertility.Rate + .1, text := ~Hover.Label, fontWeight := 'bold',
               fontSize := 20, fill = ~Region, stroke := 'black') %>%
    layer_text(x = ~Life.Expectancy - 1.75, y = 0.3, text := ~X.Label, fontWeight := 'bold', fontSize := 12,
               fill := 'black') %>% 
    layer_points(x = ~Hover.X.Life, y = ~Hover.Y.Life, shape := 'triangle-down', fill = ~Region, opacity = ~Hover.Y.Life*10) %>% 
    layer_text(x = 8, y = ~Fertility.Rate - .1, text := ~Y.Label, fontWeight := 'bold', fontSize := 12,
               fill := 'black') %>% 
    layer_points(x = ~Hover.X.Fertility, y = ~Hover.Y.Fertility, shape := 'diamond', fill = ~Region, opacity = ~Hover.X.Fertility/6) %>% 
    hide_legend(c("size", "stroke", "fill")) %>% 
    bind_shiny('plot1')
}

shinyApp(ui = ui, server = server)
