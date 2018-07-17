# File contains methoods and queries related to data extraction and processing 
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
  'adds a seasons column to the data according to the south african seasons'
  df1$season <- df1$month
  
  df1[df1$season=="Dec"| df1$season=="Jan"| df1$season== "Feb","season"] <- "summer"
  df1[df1$season=="Mar"| df1$season=="Apr"| df1$season== "May","season"] <- "autumn"
  df1[df1$season=="Jun"| df1$season=="Jul"| df1$season== "Aug","season"] <- "winter"
  df1[df1$season=="Sep"| df1$season=="Oct"| df1$season== "Nov","season"] <- "spring"
  
  return(df1)
}



get_min_date <- "SELECT min(malaria.create_date_time) FROM malaria24.ona_reportedcase AS malaria"
get_max_date <- "SELECT max(malaria.create_date_time) FROM malaria24.ona_reportedcase AS malaria"


get_malaria_Data <-"SELECT *, 
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


