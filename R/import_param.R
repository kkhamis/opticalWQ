library(tidyverse)
library(lubridate)
library(ChemoSpec)
library(janitor)

import_param<-function(path,date_format="ymd HMS",single_sensor=T, sensor){
        ###----initial data extraction------------------------
        if(single_sensor==T){
        files <- dir(path = path,recursive = T,
                     pattern="003R",full.names = T)

         df<-files %>% map(~read_delim(.x,
             delim = ";", na = "#NV" )) %>%
             reduce(rbind) %>% clean_names() %>%
             filter(!str_detect(name, "(scaled)")) %>%
                 time_setup() %>%
                 select(-starts_with("comment"))

         df_scaled<-files %>% map(~read_delim(.x,
                delim = ";", na = "#NV" )) %>%
                 reduce(rbind) %>% clean_names() %>%
                 filter(str_detect(name, "(scaled)")) %>%
                 time_setup() %>%
                 select(-starts_with("comment"))

         list(df=df,df_scaled=df_scaled)

        } else if (single_sensor==F){

                ls<-list()
                for(i in 1:length(sensor)){
        files <- dir(path = path,recursive = T,
                pattern=sensor[i],full.names = T)
           df<-files %>% map(~read_delim(.x,
            delim = ";", na = "#NV" )) %>%
           reduce(rbind) %>% clean_names() %>%
          filter(!str_detect(name, "(scaled)")) %>%
          time_setup() %>%
          select(-starts_with("comment"))

         df_scaled<-files %>% map(~read_delim(.x,
                 delim = ";", na = "#NV" )) %>%
                reduce(rbind) %>% clean_names() %>%
          filter(str_detect(name, "(scaled)")) %>%
          time_setup() %>%
            select(-starts_with("comment"))
          ls[[i]]<-list(df=df,df_scaled=df_scaled)
               names(ls)[[i]]  <- sensor[i]

                }
                list(sensor_list=ls)
        }
         }



test1<-import_param(path = "D:/Uni work/Postdoc 2019-2020/Field work/India field_Patna-2019/Trios/All_Nov_trios data/DATA/OPUS",single_sensor = T)

test2<-import_param(path = "D:/Uni work/Postdoc 2019-2020/Field work/India field_Patna-2019/Trios/opus cal 2019",single_sensor = F,sensor=c("R7213","R7214","R7215"))

