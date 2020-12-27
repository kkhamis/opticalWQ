
time_setup<-function(df, parse="ymd HMS",round="30 minutes"){
        df$TS<-lubridate::parse_date_time(paste(df$date_time), parse)
        df<-dplyr::mutate(df, h=lubridate::hour(TS))
        df<-dplyr::mutate(df, min=lubridate::minute(TS))
        df<-dplyr::mutate(df, doy=lubridate::yday(TS))
        df<-dplyr::mutate(df, TS_round=lubridate::round_date(TS, round))
        df<-dplyr::mutate(df, h_min=paste(h,":",min))
}
