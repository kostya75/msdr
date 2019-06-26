

# part 1: read data =====
# https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1


library(dplyr)
library(tidyr)

library(readr)
data_df<-read_delim(file="signif.txt", col_names = TRUE,delim="\t",trim_ws = T,
             col_types=cols(
                  I_D = col_double(),
                  FLAG_TSUNAMI = col_character(),
                  YEAR = col_double(),
                  MONTH = col_double(),
                  DAY = col_double(),
                  HOUR = col_double(),
                  MINUTE = col_double(),
                  SECOND = col_double(),
                  FOCAL_DEPTH = col_double(),
                  EQ_PRIMARY = col_double(),
                  EQ_MAG_MW = col_double(),
                  EQ_MAG_MS = col_double(),
                  EQ_MAG_MB = col_double(),
                  EQ_MAG_ML = col_double(),
                  EQ_MAG_MFA = col_double(),
                  EQ_MAG_UNK = col_double(),
                  INTENSITY = col_double(),
                  COUNTRY = col_character(),
                  STATE = col_character(),
                  LOCATION_NAME = col_character(),
                  LATITUDE = col_double(),
                  LONGITUDE = col_double(),
                  REGION_CODE = col_double(),
                  DEATHS = col_double(),
                  DEATHS_DESCRIPTION = col_double(),
                  MISSING = col_integer(),
                  MISSING_DESCRIPTION = col_double(),
                  INJURIES = col_double(),
                  INJURIES_DESCRIPTION = col_double(),
                  DAMAGE_MILLIONS_DOLLARS = col_double(),
                  DAMAGE_DESCRIPTION = col_double(),
                  HOUSES_DESTROYED = col_double(),
                  HOUSES_DESTROYED_DESCRIPTION = col_double(),
                  HOUSES_DAMAGED = col_double(),
                  HOUSES_DAMAGED_DESCRIPTION = col_double(),
                  TOTAL_DEATHS = col_double(),
                  TOTAL_DEATHS_DESCRIPTION = col_double(),
                  TOTAL_MISSING = col_integer(),
                  TOTAL_MISSING_DESCRIPTION = col_integer(),
                  TOTAL_INJURIES = col_double(),
                  TOTAL_INJURIES_DESCRIPTION = col_double(),
                  TOTAL_DAMAGE_MILLIONS_DOLLARS = col_double(),
                  TOTAL_DAMAGE_DESCRIPTION = col_double(),
                  TOTAL_HOUSES_DESTROYED = col_double(),
                  TOTAL_HOUSES_DESTROYED_DESCRIPTION = col_double(),
                  TOTAL_HOUSES_DAMAGED = col_double(),
                  TOTAL_HOUSES_DAMAGED_DESCRIPTION = col_double()
                )
               
             )

# eq_location_clean() =====
# that cleans the LOCATION_NAME column by stripping out the country name (including the colon) and converts names to title case (as opposed to all caps). 
# This will be needed later for annotating visualizations. 
# This function should be applied to the raw data to produce a cleaned up version of the LOCATION_NAME column.

# Input 1: dataframe, column name
# output: data frame with updated column name

eq_location_clean<-function(df,column){
  df[[column]]<-stringr::str_to_title(sub(".*(\\:)\\s+","",df[[column]]))
  return(df)
}


data_df<-eq_location_clean(data_df,"LOCATION_NAME")

convert_na_1<-function(x) ifelse(!is.na(x),x,1)
convert_na_n<-function(x,n) ifelse(!is.na(x),x,n)

# Add datetime
data_df2<-
data_df%>%
  mutate(MONTH=convert_na_1(MONTH), DAY=convert_na_1(DAY),HOUR=convert_na_1(HOUR),MINUTE=convert_na_1(MINUTE))%>%
  unite(datetime,YEAR, MONTH, DAY,HOUR,remove=F)%>%
  mutate(datetime=lubridate::ymd_h(datetime))%>%
  select(-c(MONTH,DAY,HOUR,MINUTE,SECOND))

