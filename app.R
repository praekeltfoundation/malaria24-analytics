library(shiny)
library(lubridate)
library(leaflet)
library(magrittr)
library(rjson)
source("mydata.R")


 
#get malaria data from local dir if it exists otherwise get from database and save to dir
if ("malaria_data.csv" %in% list.files()){
  malaria_data <-  read.csv("malaria_data.csv", header=TRUE)
  malaria_data[,"X"] <- NULL
  
  malaria_data$date_reported<-as.Date(malaria_data$date_reported)
  min_date <- substr(min(malaria_data$date_reported), 1, 10)
  max_date <- substr(max(malaria_data$date_reported), 1, 10)
  
} else {
  json_file <- fromJSON(file=file.path(Sys.getenv("HOME"), "credentials.json"))
  
  malaria_data <- get_data_fromDB(json_data=json_file, get_malaria_Data_sqlquery) %>% 
    get_seasons()
  
  min_date <- substr(min(malaria_data$date_reported), 1, 10)
  max_date <- substr(max(malaria_data$date_reported), 1, 10)
  write.csv(malaria_data, file = "malaria_data.csv")

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
                      sidebarLayout(
                        sidebarPanel(
                          
                          imageOutput("malaria_news")
                        ),
                        mainPanel(
                          plotOutput("monthlyCases_by_year")
                          #,plotOutput("timeSeriesGraph")
                         ))
                      ),
             tabPanel("Summary", 
                      tableOutput("reported_cases"), 
                      tableOutput("abroad"))
             )


server <- function(input, output, session) {
  output$malaria_news <- renderImage({
    
    list(src = "malaria_news.png",
         contentType = 'image/png',
         width = 400,
         height = 400)
  }, deleteFile = FALSE)
  
  #plot of total malaria cases for each month in a particular year
  output$monthlyCases = renderPlot({
    
    counts <- malaria_data %>%
      get_all_monlthly_malaria_counts()

    barplot(counts, beside=T, legend=rownames(counts),
            main="Reported Malaria Cases in South Africa",xlab="Month of Year", ylab="Reported Cases")
   
  })
  
  output$monthlyCases_by_year <-renderPlot({
    monthly_malaria_counts <- malaria_data[,c("date_reported", "month")]
    monthly_malaria_counts$date_reported <- year(monthly_malaria_counts$date_reported) 
    month_names <- c("Jan","Feb","Mar","Apr","May", "Jun","Jul","Aug","Sep", "Oct", "Nov","Dec")
    month_names <-factor(month_names, levels=month_names)
  
    monthly_malaria_counts2015 <- monthly_malaria_counts %>%
      get_year_monthly_malaria_counts(report_year="2015", col2="count")
    monthly_malaria_counts2015$month <- month_names[as.numeric(levels(droplevels(monthly_malaria_counts2015$month)))]
    
    monthly_malaria_counts2016 <- monthly_malaria_counts %>%
      get_year_monthly_malaria_counts(report_year="2016", col2="count")
    monthly_malaria_counts2016$month <- month_names[as.numeric(levels(droplevels(monthly_malaria_counts2016$month)))]
    
    monthly_malaria_counts2017 <- monthly_malaria_counts %>%
      get_year_monthly_malaria_counts(report_year="2017", col2="count")
    monthly_malaria_counts2017$month <- month_names[as.numeric(levels(droplevels(monthly_malaria_counts2017$month)))]
    
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

