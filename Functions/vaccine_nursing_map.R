library(sf)
library(tigris)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(shiny)
library(dplyr)
library(lubridate)

load("Data/nurse_categoryRate.csv")

nurse_deathRate = nurse_categoryRate %>%
    drop_na(county_deathRate) %>%
    dplyr::select(date,
                  state_county, 
                  county_deathRate)
    

options(tigris_use_cache = TRUE)

counties_list = counties(cb = TRUE,
                         resolution = "500k") %>%
    shift_geometry() %>%
    dplyr::filter(as.numeric(STATEFP) %in% 1:56) %>%
    mutate(state_county = tolower(paste(STATE_NAME, NAME, sep = ","))) %>%
    left_join(nurse_deathRate,
               by = "state_county")
    # mutate(date1 = format(date, "%d-%b-%Y")) %>%
    # drop_na()



states_list = states(cb = TRUE,
                     resolution = "500k") %>%
    shift_geometry() %>%
    dplyr::filter(as.numeric(STATEFP) %in% 1:56)

counties_list_shifted = counties_list %>%
    filter(date == "2022-04-03")

g = ggplot() +
    geom_sf(data = counties_list,
            size = .2,
            aes(fill = county_deathRate)) +
    
    geom_sf(data = states_list,
                color = "black",
                fill = NA,
                size = .3) +
    #scale_fill_viridis_c(option = "I",
                         #direction = 1)+
    scale_fill_gradientn(colors = c("green", "red"),
        values = NULL,
        space = "Lab",
        na.value = "grey",
        guide = "colourbar",
        aesthetics = "fill"
    ) +
    transition_manual(date) + 
    ggtitle("Death Rate in nurse homes in county level ")

anim_p2 = animate(g, fps = 8,
                  start_pause = 1,
                  end_pause = 15,
                  detail = 2,
                  rewind = FALSE,
                  width = 720,
                  height = 720,
                  res = 140,
                  renderer = gifski_renderer())
anim_save(filename = "covid_aus_cases_EMA_aug30_.gif",
          animation = g)
plot(g)


# save as a GIF
animate(nations_plot,
        fps = 10,
        width = 750,
        height = 450)
anim_save("nations.gif")

# save as a video
animate(g,
        renderer = ffmpeg_renderer(),
        width = 800,
        height = 800)
anim_save("Covid.mp4")



########################################################
map_with_animation <- g +
    transition_time(date) +
    ggtitle('date: {frame_time}',
            subtitle = 'Frame {frame} of {nframes}')
date <- max(nurse_deathRate$date) - min(nurse_deathRate$date) + 1
animate(map_with_animation, nframes = date)
animate(g, nframes = 20, fps = 4, renderer = av_renderer())
anim_save("example2.mpg")



date
ggsave("Result/counties_map.jpg",
       g,
       height=4,width=8,scale=1.65)






ui = fluidPage(
    sidebarPanel(
        sliderInput(inputId = "date",
                    label = "Year of casualty:",
                    value = 2020-05-24,
                    min = 2020-05-24,
                    max = 2022-05-01)),
    # Show a plot of the generated distribution
    mainPanel(
        leafletOutput("map")
    )
)

# Define server logic
server <- shinyServer(function(input, output) {
    p = readRDS("vspd-data/ac_cycle_lnd.Rds")
    output$map = renderLeaflet({
        leaflet() %>%
            addProviderTiles("Thunderforest.OpenCycleMap") %>% 
            setView(lng = -0.1, lat = 51.5, zoom = 10)
    })
    observe({
        leafletProxy("map") %>% clearShapes()%>%
            addCircles(data = p[grepl(input$year, p$Date),])
    })
})

# Run the application 
shinyApp(ui, server)


library(tidyverse)
library(shiny)

sampledf <- tibble(Name = c("John Smith"),
                   'Test 1' = c("Test 1"),
                   'Date' = lubridate::as_date(c("2020-04-22",
                                                 "2020-04-22",
                                                 "2020-04-22",
                                                 "2020-04-24",
                                                 "2020-04-24",
                                                 "2020-04-24",
                                                 "2020-04-24",
                                                 "2020-04-26",
                                                 "2020-04-26",
                                                 "2020-04-26")),
                   'Result 1' = rnorm(1:10),
                   'Result 2' = rnorm(1:10),
                   'Result 3' = rnorm(1:10))

# Define UI for application
ui <- navbarPage(
    "Title",
    
    tabPanel(
        "Tab 1",
        sidebarPanel(
            h4("Inputs"),
            selectInput(
                "Name_Select",
                label = "Select Name",
                choices = sampledf$Name,
                selected = TRUE
            ),
            dateRangeInput(
                "dates",
                label = "Dates",
                start = min(sampledf$Date),
                end = max(sampledf$Date)
            ),
            varSelectInput("X_Axis",
                           label = "Select Variable 1",
                           data = sampledf,
            ),
            varSelectInput("Y_Axis",
                           label = "Select Variable 2",
                           data = sampledf,
            ),
        )
    ),
    
    mainPanel(plotOutput("plot")),
    
    tabPanel("Tab2")
    
)

# Define server logic
server <- function(input, output) {
    
    output$plot <- renderPlot({
        Data %>% 
            filter(sampledf$Date >= input$dates[1], sampledf$Date <= input$dates[2]) %>%
            ggplot(mapping = (aes_string(x = input$X_Axis, y = input$Y_Axis))) +
            geom_line(color = input$date) +
            geom_point(color = input$date)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)






##########################Histogram Example

library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Hello Shiny!"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Slider for the number of bins ----
            sliderInput(inputId = "bins",
                        label = "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Histogram ----
            plotOutput(outputId = "distPlot")
            
        )
    )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    # Histogram of the Old Faithful Geyser Data ----
    # with requested number of bins
    # This expression that generates a histogram is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$distPlot <- renderPlot({
        
        x    <- faithful$waiting
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x, breaks = bins, col = "#75AADB", border = "white",
             xlab = "Waiting time to next eruption (in mins)",
             main = "Histogram of waiting times")
        
    })
    
}

shinyApp(ui = ui, server = server)

############################Animation

library(ggplot2)
library(gganimate)

ggplot(mtcars, aes(factor(cyl), mpg)) + 
    geom_boxplot() + 
    # Here comes the gganimate code
    transition_states(
        gear,
        transition_length = 2,
        state_length = 1
    ) +
    enter_fade() + 
    exit_shrink() +
    ease_aes('sine-in-out')


library(gapminder)
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
    geom_point(alpha = 0.7, show.legend = FALSE) +
    scale_colour_manual(values = country_colors) +
    scale_size(range = c(2, 12)) +
    scale_x_log10() +
    facet_wrap(~continent) +
    # Here comes the gganimate specific bits
    labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
    transition_time(year) +
    ease_aes('linear')
