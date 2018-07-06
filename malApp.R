library(shiny)

ui <- fluidPage(
  #titlePanel("Malaria Stats"),

  navbarPage(tabPanel("Malaria Stats",
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
                                   "North West" = "nw")
                         )),
             tabPanel("Map Overview", plotOutput("map")),
             tabPanel("Graphing", plotOutput("timeSeriesGraph")),
             tabPanel("Summary", tableOutput("summaryStats"))
             )#,

  # mainPanel(
  #   tabsetPanel(
  #     tabPanel("Map Overview", plotOutput("map"), tableOutput("summaryStats")),
  #     tabPanel("Graphing", plotOutput("timeSeriesGraph")),
  #     tabPanel("Summary", plotOutput("summaryStats"))
  #     
  #   ))
  )


server <- function(input, output, session) {
  require(ggmap)
  require(ggplot2)
  source("mydata.R")
  credentials = read.csv("/home/mkhuphuli/hello/credentials.csv", header=TRUE)
  

  output$summaryStats = renderTable({
    
    mal_data <- get_data_fromDB(credentials=credentials, get_malaria_Data)
    df1 <- reported_case_counts(df=mal_data)
    
    if(input$province=='all_p'){
      #get reported cases by province
      freq_table <- as.data.frame(table(df1$province))
      freq_table <- freq_table[order(freq_table$Freq),] #order by frequency
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
  
  
  output$map = renderPlot({
    
    mal_data <- get_data_fromDB(credentials=credentials, get_malaria_Data)
    df1 <- reported_case_counts(df=mal_data)
  
    if(input$province=='all_p'){
      df<- na.omit(df1[, c("longitude", "latitude")]) 
      zoom <- 5
    } else {
      df1 <- df1[df1$province==input$province,]
      df <- na.omit(df1[ c("longitude", "latitude")])
      zoom <- 6
    }

    gps_of_provinces <- read.csv("gps_of_provinces.csv", header=TRUE)
    row.names(gps_of_provinces)<-c("lon","lat")
    

    if(nrow(df1[df1$province==input$province,])==0 & input$province!="all_p"){
      #no data in province
      mymap <- get_map(location= c(lon=gps_of_provinces['lon',"all_p" ],lat=gps_of_provinces['lat', "all_p" ]),  
                     maptype = "roadmap",zoom=zoom, scale=2)
      ggmap(mymap) 
      
      } else {
        mymap <- get_map(location= c(lon=gps_of_provinces['lon',input$province ],lat=gps_of_provinces['lat',input$province ]),  
                     maptype = "roadmap",zoom=zoom, scale=2)
        ggmap(mymap) + 
          geom_point(data= df, aes(x=longitude, y=latitude, fill="red", alpha = 0.8), size=1, shape=21) +
          guides(fill=FALSE, alpha=FALSE, size=FALSE)
        }
  }, width=1000,height=1000)
  
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

