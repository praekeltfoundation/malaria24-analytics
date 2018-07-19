# File contains methods and queries related to data extraction and processing 
get_data_fromDB<-function(credentials=credentials, sqlquery){
  "Connects to intern database.  
  credentilas= json file with dbname,host,port, user,password"
  
  require("RPostgreSQL")
  require("nnet")
  
  #connect to the intern malaria database
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = as.character(credentials$dbname),
                   host = as.character(credentials$host), port = credentials$port,
                   user = as.character(credentials$user), password = as.character(credentials$password)
  )
  rm(credentials)
  #get data set and close connection
  data <- dbGetQuery(con, sqlquery)
  dbDisconnect(con)
  
  return(data)

}

get_seasons <- function(df) {
  library(dplyr)
  "adds a seasons column to the data according to the south african seasons
  mutate evaluates evaluates expression in order semilar to if else"
  
  df %>%
    mutate(season = case_when(month %in% c("Dec", "Jan", "Feb") ~ "summer",
                              month %in% c("Mar", "Apr", "May") ~ "autumn",
                              month %in% c("Jun", "Jul", "Aug") ~ "winter", 
                              month %in% c("Sep", "Oct", "Nov") ~ "spring"))
  
  return(df)
}
get_year_monthly_malaria_counts <- function(monthly_malaria_counts, report_year, col2="count"){
  
  report_year <- report_year %>% as.character()
  monthly_malaria_counts2015 <- monthly_malaria_counts[monthly_malaria_counts$date_reported==report_year, c("month")] %>%
    table() %>%
    data.frame()
  names(monthly_malaria_counts2015) <- c("month", col2)
  
  return(monthly_malaria_counts2015)
}


get_all_monlthly_malaria_counts <- function(malaria_data) {
  
  monthly_malaria_counts <- malaria_data[,c("date_reported", "month")]
  monthly_malaria_counts$date_reported <- year(monthly_malaria_counts$date_reported) 
  
  # get monthly malaria counts for 2015
  monthly_malaria_counts2015 <- monthly_malaria_counts %>%
    get_year_monthly_malaria_counts(report_year="2015", col2="2015") 
  
  #get monthly malaria counts for 2016
  monthly_malaria_counts2016 <- monthly_malaria_counts %>%
    get_year_monthly_malaria_counts(report_year="2016", col2="2016") 
  
  #get monlthy malaria counts for 2017
  monthly_malaria_counts2017 <- monthly_malaria_counts %>%
    get_year_monthly_malaria_counts(report_year="2017", col2="2017") 
  
  # merge malaria counts for all the years into one data frame
  counts <- merge(monthly_malaria_counts2015, monthly_malaria_counts2016, all=TRUE, by="month") %>%
    merge(monthly_malaria_counts2017, all=TRUE, by="month")
  counts[is.na(counts)] <- 0
  
  #transpose and convert the data into a matrix with years being row names and months as column names
  counts <-counts[,c("2015","2016", "2017")] %>%
    t() %>%
    data.matrix()
  colnames(counts) <- c("Jan","Feb","Mar","Apr","May", "Jun","Jul","Aug","Sep", "Oct", "Nov","Dec")
  
  return(counts)
}



get_malaria_Data_sqlquery <-"SELECT *, 
                    EXTRACT ('month' FROM date_reported) as month
                    FROM
                      (SELECT 
                       malaria.create_date_time AS date_reported,  malaria.locality,malaria.abroad, malaria.gender, 
                       clinic.longitude, clinic.latitude, substring(clinic.district from 4) AS district, 
                       substring(clinic.district from 1 for 2) AS province, clinic.subdistrict, clinic.facility
                       FROM malaria24.ona_reportedcase AS malaria
                       LEFT JOIN
                       clinic_schema.clinic_database_updated_20160929 AS clinic
                       ON malaria.facility_code::text = clinic.facilitycode::text) AS malaria_reports
                                 "

get_time_series_Data_sqlquery <- "WITH weekly_count_table AS 
                  (
                  SELECT 
                  to_char(malaria.create_date_time, 'YYYY') AS year, 
                  to_char(malaria.create_date_time, 'WW') AS week_of_year, 
                  clinic.district,
                  COUNT(*) as count_of_incidents
                  FROM
                  malaria24.ona_reportedcase AS malaria
                  LEFT JOIN 
                  clinic_schema.clinic_database_updated_20160929 AS clinic
                  ON malaria.facility_code::text = clinic.facilitycode::text
                  GROUP BY district,year,week_of_year
                  ORDER BY district,year,week_of_year
                  ),
                  facility_averages AS
                  (
                  SELECT
                  year,
                  district, 
                  week_of_year,
                  count_of_incidents
                  FROM
                  weekly_count_table
                  
                  )
                  SELECT
                  year,
                  district,
                  week_of_year,
                  lag(count_of_incidents,2) OVER (ORDER BY district,year ASC, week_of_year ASC) AS now_2,
                  lag(count_of_incidents,1) OVER (ORDER BY district,year ASC, week_of_year ASC) AS now_1,
                  lag(count_of_incidents,0) OVER (ORDER BY district,year ASC, week_of_year ASC) AS now_0
                  FROM facility_averages
                  ORDER BY district, year"


