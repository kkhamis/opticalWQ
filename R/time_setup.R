
library(lubridate)
time_setup<-function(df, parse="ymd HMS"){
        df$TS<-parse_date_time(paste(df$DateTime), parse)
        df$TS<-df$TS +hours(4)+minutes(30)
        df<-mutate(df, h=lubridate::hour(TS))
        df<-mutate(df, min=lubridate::minute(TS))
        df<-mutate(df, doy=lubridate::yday(TS))
        df<-mutate(df, TS_round=round_date(TS, "30 minutes")) ###nice function for rounding to the nearest 5 mins!!!
        df<-mutate(df, h_min=paste(h,":",min))
}
