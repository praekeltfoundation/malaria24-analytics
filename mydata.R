#read in credentials
credentials <- read.csv("/home/mkhuphuli/hello/credentials.csv", header=TRUE)

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

get_min_date <- "SELECT min(malaria.create_date_time) FROM malaria24.ona_reportedcase AS malaria"
get_max_date <- "SELECT max(malaria.create_date_time) FROM malaria24.ona_reportedcase AS malaria"


get_malaria_Data <-"SELECT *, 
                    EXTRACT ('month' FROM date_reported) as month
                    FROM
                      (SELECT 
                       malaria.create_date_time AS date_reported,  malaria.locality,malaria.abroad, malaria.gender, 
                       clinic.longitude, clinic.latitude, clinic.district, clinic.subdistrict, clinic.facility
                       FROM malaria24.ona_reportedcase AS malaria
                       LEFT JOIN
                       clinic_schema.clinic_database_updated_20160929 AS clinic
                       ON malaria.facility_code::text = clinic.facilitycode::text) AS malaria_reports
                                 "

get_time_Data <- "WITH weekly_count_table AS 
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

reported_case_counts <- function(df=get_malaria_Data ){
  library(lubridate)
  #detach province names from disrtrict, subdistrict and facilitydistrict
  df$province <- substr(df$district, 1, 2)
  df$district <- substr(df$district, 4, nchar(df$district))
  df$subdistrict <- substr(df$subdistrict, 4, nchar(df$subdistrict))
  df$facility <- substr(df$facility, 4, nchar(df$facility)) 
  
  #create 'date_reported'(local time zone) variable and remove 'create_date_time' variable 
  #df$date_reported <- ymd_hms(df$create_date_time, tz="Africa/Johannesburg")
  #df$create_date_time<-NULL 
  
  #select 2017 data
  #df$case <- replicate(100,1)
  #df2 <- subset(df, year(date_reported) == '2017')
  #df2$date_reported <- floor_date(df2$date_reported, "day")
  #need to aggregate daily cases or weekly case
  
  return(df)
  
}

get_seasons <- function(df1, date_column) {
  
  #get number of records
  rows <- length(df1[, date_column])
  #initialise season column
  df1$season <- 1:rows
  df1$month <- 1:rows
  #allocate seasons according to month name and also return month names
  for (i in 1:rows) {
    if((month(df1[i, date_column], label=TRUE)>="Sep") & (month(df1[i, date_column], label=TRUE)<="Nov")){
      df1$season[i] <- "spring"
      df1$month[i] <- month(df1[i, date_column],  label=TRUE)
    } else if((month(df1[i, date_column], label=TRUE)>="Mar") & (month(df1[i, date_column], label=TRUE)<="May")){
      df1$season[i]  <- "autumn"
      df1$month[i] <- month(df1[i, date_column],  label=TRUE)
    } else if((month(df1[i, date_column], label=TRUE)>="Jun") & (month(df1[i, date_column], label=TRUE)<="Aug")){
      df1$season[i]  <- "winter"
      df1$month[i] <- month(df1[i, date_column],  label=TRUE)
    } else {
      df1$season[i]  <- "summer"
      df1$month[i]<- month(df1[i, date_column],  label=TRUE)
    }
  }
  return(df1)
}

