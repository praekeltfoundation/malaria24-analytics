library(shiny)
library(lubridate)
library(leaflet)
library(magrittr)
source("mydata.R")

credentials <- read.csv("/home/mkhuphuli/hello/credentials.csv", header=TRUE) 
#get the first date on which a malaria case was recorded
min_date <- read.csv("/home/mkhuphuli/hello/credentials.csv", header=TRUE) %>%
  get_data_fromDB(get_min_date_sqlquery)
min_date<- substr(min_date[1,"min"], 1, 10)

# get the last date on which a malaria case was recorded
max_date <- read.csv("/home/mkhuphuli/hello/credentials.csv", header=TRUE) %>%
  get_data_fromDB(get_max_date_sqlquery)
max_date<- substr(max_date[1,"max"], 1, 10)
 
#get malaria data from cache if it exists otherwise get from database and save to cache
if ("malaria_data.csv" %in% list.files()){
  malaria_data <-  read.csv("malaria_data.csv", header=TRUE)
  malaria_data[,"X"] <- NULL
} else {
  malaria_data <- get_data_fromDB(credentials=credentials, get_malaria_Data_sqlquery) %>% 
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
            
             tabPanel("Graphing",
                      plotOutput("monthlyCases_by_year"),
                      plotOutput("timeSeriesGraph")),
             tabPanel("Summary", 
                      tableOutput("reported_cases"), 
                      tableOutput("abroad"))
             )


server <- function(input, output, session) {
  
  #plot of total malaria cases for each month in a particular year
  output$monthlyCases = renderPlot({
    monthly_malaria_counts <- malaria_data[,c("date_reported", "month")]
    monthly_malaria_counts$date_reported <- year(monthly_malaria_counts$date_reported) #covert date column to just year fromat

    monthly_malaria_counts2015 <- monthly_malaria_counts[monthly_malaria_counts$date_reported=="2015", c("month")] %>%
      table() %>%
      data.frame()
    names(monthly_malaria_counts2015) <- c("month", "2015")
    
    monthly_malaria_counts2016 <- monthly_malaria_counts[monthly_malaria_counts$date_reported=="2016", c("month")] %>%
      table() %>%
      data.frame()
    names(monthly_malaria_counts2016) <- c("month", "2016")
    
    monthly_malaria_counts2017 <- monthly_malaria_counts[monthly_malaria_counts$date_reported=="2017", c("month")] %>%
      table() %>%
      data.frame()
    names(monthly_malaria_counts2017) <- c("month", "2017")
    
    counts <- merge(monthly_malaria_counts2015, monthly_malaria_counts2016, all=TRUE, by="month") %>%
      merge(monthly_malaria_counts2017, all=TRUE, by="month")
    counts[is.na(counts)] <- 0
    counts <-counts[,c("2015","2016", "2017")] %>%
      t() %>%
      data.matrix()
    colnames(counts) <- c("Jan","Feb","Mar","Apr","May", "Jun","Jul","Aug","Sep", "Oct", "Nov","Dec")
      
    barplot(counts, beside=T, legend=rownames(counts),
            main="Reported Malaria Cases in South Africa",xlab="Month of Year", ylab="Reported Cases")
   
  })
  
  output$monthlyCases_by_year <-renderPlot({
    monthly_malaria_counts <- malaria_data[,c("date_reported", "month")]
    monthly_malaria_counts$date_reported <- year(monthly_malaria_counts$date_reported) #covert date column to just year fromat
  
    monthly_malaria_counts2015 <- monthly_malaria_counts[monthly_malaria_counts$date_reported=="2015", c("month")] %>%
      table() %>%
      data.frame()
    names(monthly_malaria_counts2015) <- c("month", "count")
    
    monthly_malaria_counts2016 <- monthly_malaria_counts[monthly_malaria_counts$date_reported=="2016", c("month")] %>%
      table() %>%
      data.frame()
    names(monthly_malaria_counts2016) <- c("month", "count")
    
    monthly_malaria_counts2017 <- monthly_malaria_counts[monthly_malaria_counts$date_reported=="2017", c("month")] %>%
      table() %>%
      data.frame()
    names(monthly_malaria_counts2017) <- c("month", "count")
    #plot reported malaria cases by month
    par(mfrow=c(2,2)) 
    boxplot(count ~ month, data=monthly_malaria_counts2015,
            main="Reported Malaria Cases in South Africa 2015", ylab="Reported Cases", xlab="Month of Year")
    boxplot(count ~ month, data=monthly_malaria_counts2016,
            main="Reported Malaria Cases in South Africa 2016", ylab="Reported Cases", xlab="Month of Year")
    boxplot(count ~ month, data=monthly_malaria_counts2017,
            main="Reported Malaria Cases in South Africa 2017", ylab="Reported Cases", xlab="Month of Year")
  })
  
  
  #print to screen the year and moonth of selected date from slider
  output$yearmonth = renderText({
    month_of_year <- paste(year(input$date), month(input$date, label=TRUE))
    })
  
  output$abroad = renderTable({
    #make table showing total number of reported case by different coutries patients travelled too
    abroad <- as.data.frame(table(malaria_data [,"abroad"]))
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
   
    #get subset of data according to provinc, return cumulative or monthly data accordingly
    if(input$province=="all_p"){
        zoom <- 5
        if(input$slider_data=="cum_total"){
            malaria_data <- malaria_data[as.Date(malaria_data$date_reported) <= input$date,]
        } else {
            malaria_data <- malaria_data[format(as.Date(malaria_data$date_reported), "%Y-%m") == format(input$date, "%Y-%m"),]
        }
    } else {
        malaria_data <- malaria_data[malaria_data$province==input$province,]
        zoom <- 6
        if(input$slider_data=="cum_total"){
            malaria_data <- malaria_data[as.Date(malaria_data$date_reported) <= input$date,]
        } else {
            malaria_data <- malaria_data[format(as.Date(malaria_data$date_reported), "%Y-%m") == format(input$date, "%Y-%m"),]
        }
    }
    
    df <- na.omit(malaria_data[ c("longitude", "latitude")]) #remove data with missing coordinates
    gps_of_provinces <- read.csv("gps_of_provinces.csv", header=TRUE)
    row.names(gps_of_provinces)<-c("lon","lat")
    
    #get south african map and plot coordinates of malaria cases
    malaria_cases_mapplot <- leaflet(df) %>% 
      setView(lng=24.8156578, lat=-28.6628035, zoom=6) %>%
      addTiles() %>% 
      addCircles(~longitude, ~latitude, weight = 3, radius=40)
    malaria_cases_mapplot})
  
 
  output$timeSeriesGraph <- renderPlot({
    
    time_series <- get_data_fromDB(credentials=credentials, get_time_series_Data_sqlquery)
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

