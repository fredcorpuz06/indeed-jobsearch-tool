library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(shiny)
library(plotly)
library(Hmisc)
library(stringr)
library(rsconnect)
library(maps)
library(maptools)
library(raster)
library(rgdal)
library(mapproj)
library(leaflet)
library(DT)
library(zipcode)
library(tidyr)
library(RcppRoll)
library(imputeTS)
library(cowplot)


##### Shiny #####


load('datafest_data.RData')
load('when.RData')

data_when$normTitle %>% unique %>% .[!is.na(.)] -> job_titles

getJobTimePlots <- function(data, job) {
  
  if (!(job %in% data$normTitle)) {
    stop("Job not found!")
  }
  
  data_US %>%
    filter(job == normTitle) %>%
    filter(lubridate::year(date) == 2017) %>%
    left_join(industry_impute) %>%
    mutate(industry = ifelse(is.na(industry), industry_pred, industry)) %>%
    group_by(jobId, industry, estimatedSalary, educationRequirement) %>%
    arrange(date) %>%
    summarise(
      firstposting = first(date) - first(jobAgeDays),
      endposting = max(date),
      totalClicks = sum(clicks),
      totalLocalClicks = sum(clicks),
      city_state = paste(first(city), first(stateProvince), sep = ", ")
    ) %>%
    ungroup() %>%
    arrange(firstposting) -> data_to_plot
  
  max_date <- min(data_to_plot$firstposting[nrow(data_to_plot)],
                  data_to_plot$endposting[nrow(data_to_plot)])
  min_date <- max(data_to_plot$firstposting[1],
                  data_to_plot$endposting[1])
  
  print("done1")
  data_to_plot %>%
    filter(firstposting > lubridate::ymd("2017-01-01")) %>%
    dplyr::mutate(industry = str_replace(industry, ",.*", "")) %>%
    dplyr::count(firstposting) %>%
    mutate(prop = n / sum(n),
           rolling = roll_mean(prop, 7, align = "right", fill = 0)) %>%
    dplyr::select(-n) %>%
    rename(date = firstposting) %>%
    right_join(tibble(date = seq(min_date, max_date, by = "day"))) -> timefirst_ds
  
  data_to_plot %>%
    filter(endposting > lubridate::ymd("2017-01-01")) %>%
    dplyr::mutate(industry = str_replace(industry, ",.*", "")) %>%
    dplyr::count(endposting) %>%
    mutate(prop = n / sum(n),
           rolling = roll_mean(prop, 7, align = "right", fill = 0)) %>%
    dplyr::select(-n) %>% 
    rename(date = endposting) %>% 
    right_join(tibble(date = seq(min_date, max_date, by = "day"))) -> timeend_ds
  
  time_both <- full_join(timefirst_ds, timeend_ds, by = "date") %>%
    dplyr::mutate_each(funs(imputeTS::na.interpolation(., option ="spline")), -date) %>%
    dplyr::mutate(whiteout = ifelse(rolling.x < rolling.y, rolling.x, rolling.y))
  
  time_both$higher <- ifelse(time_both$rolling.x > time_both$rolling.y, "post", "end")
  
  segmenter <- function(vector) {
    value <- 1
    output <- integer(length(vector))
    output[1] <- value
    for (i in 2:length(output)) {
      if (vector[i-1] == vector[i]) {
        output[i] <- value 
      } else {
        value = value + 1
        output[i] <- value
      }
    }
    return(output)
  }
  
  time_both %>%
    mutate(month = month(date, label = T),
           day = lubridate::wday(date, label = T),
           diff = rolling.x - rolling.y,
           week = week(date)) %>% 
    group_by(month) %>%
    dplyr::mutate(monthweek = week - min(week)) %>%
    ungroup() %>%
    ggplot() +
    geom_tile(aes(monthweek, day, fill = diff), col = "white") +
    facet_wrap(~month, strip.position="top") +
    scale_fill_gradient(low = "red", high = "green") +
    scale_y_discrete(position = "top") +
    theme_classic() + 
    coord_flip() +
    labs(title = "Net flow of postings",
         x = "",
         y = "") +
    theme(legend.position = "none",
          strip.placement = "outside",
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(angle = 45)) -> calendar
  
  time_both$segment <- segmenter(time_both$higher)
  
  ggplot(time_both) +
    geom_ribbon(aes(date, ymin = rolling.x, ymax = rolling.y, fill = factor(higher), group = segment)) +
    scale_fill_manual(values = c("post" = "green", "end" = "red")) +
    geom_path(aes(date, rolling.x), color = "black") +
    geom_path(aes(date, rolling.y), color = "black") + 
    coord_cartesian(xlim = c(min_date+14, max_date-7)) +
    labs(title = "Change in flow of postings") +
    theme_classic() +
    theme(legend.position = "none",
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.y = element_blank()) -> p1
  
  data_to_plot %>%
    ungroup() %>%
    rename(date = firstposting) %>%
    filter(date > lubridate::ymd("2017-01-01")) %>%
    group_by(date) %>%
    summarise(meanSalary = mean(estimatedSalary)) %>%
    mutate(scaled = scale(meanSalary)) %>%
    mutate(rolling = roll_mean(scaled, 7, align = "right", fill = 0)) -> data_salary
  
  ggplot(data_salary) +
    geom_line(aes(date, rolling)) +
    theme_classic() +
    labs(title = "Mean salary of postings") +
    theme(legend.position = "none",
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.y = element_blank())+
    coord_cartesian(xlim = c(min_date+14, max_date-7)) -> p2 
  
  combined <- cowplot::plot_grid(calendar, p1, p2, ncol = 1, align = "h", rel_heights = c(3, 1, 1))
  return(combined)
}



#### New UI
ui <-navbarPage("Indeed.",
        tabPanel("Where?",
            sidebarLayout(
              sidebarPanel(
                #City Size
                sliderInput("city", label=h3("City Size"), 500, 3000000, c(500,3000000), 
                            step = 100000, round = T, 
                            format = "#,##0.", locale = "us", 
                            ticks = TRUE, animate = FALSE),
                #Average Home Price
                sliderInput("home", label=h3("Average Home Price"), 20000, 3000000, c(20000,3000000), 
                            step = 100000, round = T, 
                            format = "#,##0.", locale = "us", 
                            ticks = TRUE, animate = FALSE),
                #Age Selectors
                checkboxGroupInput("young", label=h3("Percentage of People 21-30 Years Old"), 
                                   choices = list("Below Average"=1, "Average"=2, "Above Average"=3), 
                                   selected = c(1,2,3)),
                checkboxGroupInput("old", label=h3("Percentage of People Over 65 Years Old"), 
                                   choices = list("Below Average"=1, "Average"=2, "Above Average"=3), 
                                   selected = c(1,2,3)),
                
                #Needed Education
                checkboxGroupInput("education", label=h3("Education Needed"), 
                                   choices = list("None"=1, "High School"=2, "Higher Education"=3), 
                                   selected = c(1,2,3)),
                #Needed Experience
                sliderInput("experience", label=h3("Years of Experience Required"), 0, 20, c(0,20), 
                            step = 1, round = T, 
                            format = "#,##0.", locale = "us", 
                            ticks = TRUE, animate = FALSE),
                #Average Rating
                sliderInput("rating", label=h3("Average Overall Company Rating"), 0, 5, c(0,5), 
                            step = 1, round = T, 
                            format = "#,##0.", locale = "us", 
                            ticks = TRUE, animate = FALSE),
                #Industry
                selectizeInput("industry",label=h3("Choose Industry"), 
                               choices=unique(us_data$industry_pred), multiple=TRUE,
                               selected= unique(us_data$industry_pred)),
                
                verbatimTextOutput("info")
              ),
              mainPanel(
                leafletOutput("map"),
                DT::dataTableOutput("table")
              )
            )     
          ),
      tabPanel("When?",
               selectizeInput("company",
                              "Type a job title:", 
                              selected = "data scientist",
                              choices=unique(job_titles),
                              options = list(maxOptions = 5)),
               plotOutput("whenPlot", height = "800px")
               
               )  
)
###


server <- function(input, output) {
  selectedData <- reactive({
    data <- us_data[which(us_data$avgOverallRating>= input$rating[1] & us_data$avgOverallRating<= input$rating[2] | is.na(us_data$avgOverallRating)),]
    data <- data[which(data$experienceRequired>= input$experience[1] & data$experienceRequired<= input$experience[2] | is.na(data$experienceRequired)),]
    data <- filter(data, educationRequirement %in% as.factor(input$education))
    #Industry filter
    data <- filter(data, industry_pred %in% c(input$industry, NA))
    c_Sal <- data.frame(city=data$city, state=data$stateProvince, salary=data$estimatedSalary)
    cities_Salary <-  aggregate(x = c_Sal, 
                               by = list(city = c_Sal$city, state=c_Sal$state), 
                               FUN = mean, na.rm=TRUE)
    cities_Salary <- cities_Salary[,c(1,2,5)]
  })
  
  stateData <-reactive({
    #sdata <- filter(state_Data, State %in% input$state)
    sdata <- state_data[which(state_data$All2>=input$city[1] & state_data$All2<=input$city[2]),]
    sdata <- sdata[which(sdata$avg_price>=input$home[1] & sdata$avg_price<=input$home[2]),]
    sdata <- filter(sdata, young %in% input$young)
    sdata <- filter(sdata, old %in% input$old)
    merge_data <- merge(sdata, selectedData(), by=c("city","state"))
    merge_data$SLI <- (merge_data$salary/merge_data$avg_price)*(merge_data$salary/merge_data$avg_price)*10
    merge_data
  })
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles( 
        urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}',
        attribution = 'Tiles &copy; Esri &mdash; Source: Esri, DeLorme, NAVTEQ, USGS, Intermap, iPC, NRCAN, Esri Japan, METI, Esri China (Hong Kong), Esri (Thailand), TomTom, 2012'
        ) %>% 
      setView(lng = -93.85, lat = 37.45, zoom = 3)
  })
  observe({    
   leafletProxy("map", data = stateData()) %>%
     clearShapes() %>%
     addCircleMarkers(~lon, ~lat, radius=~SLI, 
                stroke=FALSE, fillOpacity=0.4, fillColor=~SLI, 
                label= paste(stateData()$city, stateData()$state ,sep=", "))
   
   })
  
  output$table <- DT::renderDataTable(DT::datatable({
    meep <- stateData()[order(-stateData()$SLI), ]
    meep2 <- meep[,c(1,2,4,10)]
    names(meep2) <- c("City", "State", "Average Home Price", "Average Salary")
    meep2$`Average Home Price` <- round(meep2$`Average Home Price`)
    meep2$`Average Salary` <- round(meep2$`Average Salary`)
    head(meep2, n=50)
  }))
  
  timeInput <- reactive({
    company <- input$company
    print(company)
    p1 <- getJobTimePlots(data_when, company)
    p1
  })
  output$whenPlot <- renderPlot({
    print(timeInput())
    
    
  })

}

shinyApp(ui, server)






