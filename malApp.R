library(shiny)
library(lubridate)
library(leaflet)
#require(ggmap)
#require(ggplot2)
source("mydata.R")


#get the first date on which a malaria case was recorded
min_date <- read.csv("/home/mkhuphuli/hello/credentials.csv", header=TRUE) %>%
  get_data_fromDB(get_min_date)
min_date<- substr(min_date[1,"min"], 1, 10)

# get the last date on which a malaria case was recorded
max_date <- read.csv("/home/mkhuphuli/hello/credentials.csv", header=TRUE) %>%
  get_data_fromDB(get_max_date)
max_date<- substr(max_date[1,"max"], 1, 10)
 
#get some general malaria data 
'date_column <- "date_reported"
df1 <- get_data_fromDB(credentials=credentials, get_malaria_Data) %>%
  reported_case_counts() %>% 
  #add seasons and month according to date on which malaria case was reported
  get_seasons(date_column=date_column)'

df1 <-  read.csv("df1.csv", header=TRUE)
df1[,"X"] <- NULL
df_months <- df1[,c("date_reported", "month")]
df_months$month <- month(df_months$month, label=TRUE)
df_months$date_reported <- year(df_months$date_reported)
#seasonality <- df1[,c("date_reported", "season")]
#table(df_months[df_months$date_reported=="2016","month"]) # gives counts which what i wwant for boxplot

boxplot(Freq ~ Var1, data.frame(table(df_months[df_months$date_reported=="2016","month"])), 
        title="Reported Malaria Cases in South Africa", ylab="Reported Cases", xlab="Month of Year"
        )

ui <- navbarPage(tabPanel("Malaria Stats",
                      selectInput("province", label = "Province:",
                         choices=c("ALL" = "all_p",
                                   "Western Cape" = "wc",
                                   "Eastern Cape" = "ec",
                                   "Free State" = "fs",
                                   "Gauteng" = "gp",
                                   "Kwazulu Natal" = "kz",
                                   "limpopo" = "lp",
                                   "Mpumalanga" = "mp",
                                   "Northen Cape" = "nc",
                                   "North West" =  "nw")
                         )),
             tabPanel("Map Overview",
                      sidebarLayout(
                        #side panel with slider input to choose malaria case reported b4 specified date
                        sidebarPanel(
                          fluidRow(
                          column(sliderInput("date", "Year:",
                                      min = as.Date(min_date, "%Y-%m-%d"),
                                      max = Sys.Date(),
                                      value = as.Date(max_date),
                                      timeFormat = "%Y-%m-%d"), width=12), column(width=12),column(width=12),
                          column(tags$h1(tags$b(span(textOutput("yearmonth"), style="color:blue"))), width=5),
                          
                          column(plotOutput("monthlyCases"), width=12)
                            )),

                        #Shows map of where malaria cases are reported
                        mainPanel(
                          
                          leafletOutput("map",width = 1000, height=800))
                          )),
            
             tabPanel("Graphing", plotOutput("timeSeriesGraph")),
             tabPanel("Summary", tableOutput("reported_cases"), 
                      tableOutput("abroad"))
             )


server <- function(input, output, session) {
  
  output$monthlyCases = renderPlot({
    boxplot(Freq ~ Var1, data.frame(table(df_months[df_months$date_reported==year(input$date),"month"])), 
            main="Reported Malaria Cases in South Africa", ylab="Reported Cases", xlab="Month of Year")
  })
  
  output$yearmonth = renderText({
    txt <- paste(year(input$date), month(input$date, label=TRUE))
    })
  
  output$abroad = renderTable({
      mal_data <- get_data_fromDB(credentials=credentials, get_malaria_Data)
      abroad <- as.data.frame(table(mal_data[,"abroad"]))
      abroad <- abroad[order(abroad$Freq),]
      names(abroad) <- c("Abroad Country", "Reported Cases")
      abroad
    })

  output$reported_cases = renderTable({
    if(input$province=='all_p'){
      #get reported cases by province
      freq_table <- as.data.frame(table(df1$province))
      freq_table <- freq_table[order(freq_table$Freq),]
      names(freq_table) <- c("Province", "Reported Cases")
      freq_table
      
    } else {
      if(nrow(df1[df1$province==input$province,])==0) {
        print("\n No reported malaria cases in this Province")
      } else {
      freq_table <-table(df1[df1$province==input$province,]$subdistrict)
      freq_table <- as.data.frame(freq_table)
      freq_table <- freq_table[order(freq_table$Freq),] #order by frequency
      names(freq_table) <- c("Subdistrict", "Reported Cases")
      freq_table}
    }
  })
  
  output$map = renderLeaflet({
    
    if(input$province=="all_p"){
      df1 <- df1[substr(df1$date_reported, 1, 10) <= input$date,] # select date
      df<- na.omit(df1[, c("longitude", "latitude")]) 
      zoom <- 5
    } else {
      df1 <- df1[df1$province==input$province,]
      df1 <- df1[substr(df1$date_reported, 1, 10) <= input$date,] # select date
      df <- na.omit(df1[ c("longitude", "latitude")])
      zoom <- 6
    }
    
    gps_of_provinces <- read.csv("gps_of_provinces.csv", header=TRUE)
    row.names(gps_of_provinces)<-c("lon","lat")
    
    my_map <- leaflet(df) %>% 
      setView(lng=24.8156578, lat=-28.6628035, zoom=6) %>%
      addTiles() %>% 
      addCircles(~longitude, ~latitude, weight = 3, radius=40)
    my_map})
  
 
  output$timeSeriesGraph <- renderPlot({
    
    time_series <- get_data_fromDB(credentials=credentials, get_time_Data)
    time_series <- time_series[, c("year", "week_of_year", "district", "now_2", "now_1", "now_0")]
    time_series$province <- substr(time_series$district, 1,2) #first two charachters=province names
    time_series$district <- substr(time_series$district, 4, nchar(time_series$district))
    
    if(input$province=="all_p"){
      province_series <- time_series[, c("year", "week_of_year", "now_2", "now_1", "now_0")]
      #province_series$time <- 
    } else {
    province_series <- time_series[time_series$province==input$province,]
    }
  })
}


#fix provinces without data 
shinyApp(ui=ui, server=server)

