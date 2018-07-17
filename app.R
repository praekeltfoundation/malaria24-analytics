library(shiny)
library(lubridate)
library(leaflet)
source("mydata.R")


#get the first date on which a malaria case was recorded
min_date <- read.csv("/home/mkhuphuli/hello/credentials.csv", header=TRUE) %>%
  get_data_fromDB(get_min_date)
min_date<- substr(min_date[1,"min"], 1, 10)

# get the last date on which a malaria case was recorded
max_date <- read.csv("/home/mkhuphuli/hello/credentials.csv", header=TRUE) %>%
  get_data_fromDB(get_max_date)
max_date<- substr(max_date[1,"max"], 1, 10)
 
#get malaria data from cache if it exists otherwise get from database and save to cache
if ("malaria_data.csv" %in% list.files()){
  malaria_data <-  read.csv("malaria_data.csv", header=TRUE)
  malaria_data[,"X"] <- NULL
} else {
  malaria_data <- get_data_fromDB(credentials=credentials, get_malaria_Data) %>% 
    get_seasons()
}


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
                            
                            #gives user option to select which total  they want to see
                            column(
                              radioButtons("slider_data", "Data to show on map:",
                                                c("Cumulative Total"="cum_total",
                                                  "Monthly Total"="month_total")), width=12
                              ),
                            
                            # give suser option to select the year of the data they want to see
                            column(
                              sliderInput("date", "Year:",
                                          min = as.Date(min_date, "%Y-%m-%d"),
                                          max = Sys.Date(),
                                          value = as.Date(max_date),
                                          timeFormat = "%Y-%m-%d"), width=12
                              ),
                            
                            # shows the year and month selected by the user
                            column(
                              tags$h1(
                                tags$b(
                                  span(
                                    textOutput("yearmonth"), 
                                    style="color:blue")
                                )), width=5
                            ),
                            
                            #plots tmonthly otal number of reported cases for selected year
                            column(
                              plotOutput("monthlyCases"), width=12
                              )
                            
                            )),

                        #Shows map of where malaria cases are reported
                        mainPanel(
                          leafletOutput("map",width = 1000, height=800))
                          )
                        ),
            
             tabPanel("Graphing", plotOutput("timeSeriesGraph")),
             tabPanel("Summary", tableOutput("reported_cases"), 
                      tableOutput("abroad"))
             )


server <- function(input, output, session) {
  
  output$monthlyCases = renderPlot({
    #plot reported malaria cases by month
    boxplot(Freq ~ Var1, data.frame(table(df_months[df_months$date_reported==year(input$date),"month"])), 
            main="Reported Malaria Cases in South Africa", ylab="Reported Cases", xlab="Month of Year")
  })
  
  output$yearmonth = renderText({
    # print selected month and year
    month_of_year <- paste(year(input$date), month(input$date, label=TRUE))
    })
  
  output$abroad = renderTable({
    
    #make table showing total number of reported case by different coutries patients travelled too
      mal_data <- get_data_fromDB(credentials=credentials, get_malaria_Data)
      abroad <- as.data.frame(table(mal_data[,"abroad"]))
      abroad <- abroad[order(abroad$Freq),]
      names(abroad) <- c("Abroad Country", "Reported Cases")
      abroad
    })

  output$reported_cases = renderTable({
    # make a table showing the total reported cases by province or subdistrict
    if(input$province=='all_p'){
      #get reported cases by province
      freq_table <- as.data.frame(table(malaria_data$province))
      freq_table <- freq_table[order(freq_table$Freq),]
      names(freq_table) <- c("Province", "Reported Cases")
      freq_table
      
    } else {
      if(nrow(malaria_data[malaria_data$province==input$province,])==0) {
        print("\n No reported malaria cases in this Province")
      } else {
        # get reported cases by subdistrict
      freq_table <-table(malaria_data[malaria_data$province==input$province,]$subdistrict)
      freq_table <- as.data.frame(freq_table)
      freq_table <- freq_table[order(freq_table$Freq),] #order by frequency
      names(freq_table) <- c("Subdistrict", "Reported Cases")
      freq_table}
    }
  })
  
  output$map = renderLeaflet({
    
    #check which provices are selected and extract data accordingly
    if(input$province=="all_p"){
      malaria_data <- malaria_data[substr(malaria_data$date_reported, 1, 10) <= input$date,] # select date
      df<- na.omit(malaria_data[, c("longitude", "latitude")]) 
      zoom <- 5
    } else {
      malaria_data <- malaria_data[malaria_data$province==input$province,]
      malaria_data <- malaria_data[substr(malaria_data$date_reported, 1, 10) <= input$date,] # select date
      df <- na.omit(malaria_data[ c("longitude", "latitude")])
      zoom <- 6
    }
    
    gps_of_provinces <- read.csv("gps_of_provinces.csv", header=TRUE)
    row.names(gps_of_provinces)<-c("lon","lat")
    
    #get south african map and plot coordinates of malaria cases
    malaria_cases_mapplot <- leaflet(df) %>% 
      setView(lng=24.8156578, lat=-28.6628035, zoom=6) %>%
      addTiles() %>% 
      addCircles(~longitude, ~latitude, weight = 3, radius=40)
    malaria_cases_mapplot})
  
 
  output$timeSeriesGraph <- renderPlot({
    
    time_series <- get_data_fromDB(credentials=credentials, get_time_Data)
    time_series <- time_series[, c("year", "week_of_year", "district", "now_2", "now_1", "now_0")]
    time_series$province <- substr(time_series$district, 1,2) #first two charachters=province names
    time_series$district <- substr(time_series$district, 4, nchar(time_series$district))
    
    if(input$province=="all_p"){
      province_series <- time_series[, c("year", "week_of_year", "now_2", "now_1", "now_0")]
    } else {
    province_series <- time_series[time_series$province==input$province,]
    }
  })
}



shinyApp(ui=ui, server=server)

