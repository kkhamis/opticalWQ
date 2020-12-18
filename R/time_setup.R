
library(lubridate)
time_setup<-function(df, parse="ymd HMS",round="30 minutes"){
        df$TS<-parse_date_time(paste(df$date_time), parse)
        df<-mutate(df, h=lubridate::hour(TS))
        df<-mutate(df, min=lubridate::minute(TS))
        df<-mutate(df, doy=lubridate::yday(TS))
        df<-mutate(df, TS_round=round_date(TS, round))
        df<-mutate(df, h_min=paste(h,":",min))
}
