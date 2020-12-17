library(tidyverse)
library(lubridate)
library(ChemoSpec)

import_spectra<-function(path,date_format="ymd HMS"){
###set path high than data files
files <- dir(path = path ,recursive = T,pattern="ABSORPTI.DAT",full.names = T)
spectra<-list()
print("countdown to import complete:")
n<-length(files)
print(n)
for(k in 1:length(files)){
        test<-read_lines(files[k],skip_empty_rows = T)

        dates<-str_which(test, "Date")


        ###start and end marker for data
        dat_s<-str_which(test, "Unit4")+4
        dat_e<-dat_s+202
        ls<-list()



        for(i in 1:length(dat_s)){
                test_dat<-test[dat_s[i]:dat_e[i]]

                ###extract matrix of data
           m<-matrix(nrow = 203,ncol = 4)
         for(j in 1:length(test_dat)){
        m[j,]<-as.numeric(strsplit(test_dat, " " )[[j]] )
                }

                mat<-as_tibble(m)
                names(mat)<-c("WL","ABS","a","log_a")
                mat$a=(2.303*mat$ABS)/0.01
                mat$log_a=log((2.303*mat$ABS)/0.01)
                ls[[i]]<-mat
        }

        TS<-parse_date_time(str_sub(test[dates],start = 12,end = 30),date_format)#double check date and time format
        tib_temp<-tibble(Timestamp=TS,abs_data=ls)
        spectra[[k]]<-tib_temp
        n<-n-1
        print(n)
}
names(spectra)<-str_sub(files,start = 18,end=25)
list(spectra=spectra)
}






import_spectra2<-function(path,date_format="ymd HMS",chemspec=F){
        ###----initial data extraction------------------------
        files <- dir(path = path ,recursive = T,
                     pattern="ABSORPTI.DAT",full.names = T)
        spectra<-list()##storage list
        data<- files %>% map(~read_lines(.x,skip_empty_rows = T))
        dates<-data %>% map(~str_subset(.x,"Date"))
        TS<-map(dates,~parse_date_time(str_sub(.x,
                start = 12,end = 30),date_format))
        dat_s<-data %>% map(~str_which(.x, "Unit4")+4)
        dat_e<-data %>% map(~str_which(.x, "Unit4")+206)
        ls1<-list()##storage list1
        ls2<-list()##storage list2
        ###---------------main loop----------------------------
                for(i in 1:length(dat_s)){
                vec_l<-length(dat_s[[i]])
                ls1<-list()
                        for(j in 1:vec_l){
                 test_dat<-data[[i]][dat_s[[i]][j]:dat_e[[i]][j]]
        m<-strsplit(test_dat, " " ) %>%
        map(as.numeric) %>%  reduce(rbind)
                        mat<-as_tibble(m)
                        names(mat)<-c("WL","ABS","a","log_a")
                        mat<- mat %>% mutate(a=(2.303*ABS/0.01),
                        log_a=log((2.303*ABS)/0.01))
                        ls1[[j]]<-mat
                        }
                ls2[[i]]<-ls1
                }
        ###------------------------create tibble---------#####
tib_spectra<-tibble(file=str_sub(files,start = 18,end=25),
                    Timestamp=TS,abs_data=ls2)


###------------------------create chemspec object---------#####
if(chemspec==F){
 list(spectra_tibble=tib_spectra)
} else {
        n<-length(tib_spectra$abs_data)
        for(i in 1:n){
        test_ls[[i]]<-map(tib_spectra$abs_data[[i]],
                                 ~pluck(.x,"ABS") )
                test_ls[[i]]<-test_ls[[i]] %>% reduce(cbind)
        }

        abs_mat<-test_ls[[1]]
        for(i in 2:n){
                abs_mat<-cbind(abs_mat,test_ls[[i]])
        }
        freq<-as.numeric(tib_spectra$abs_data[[1]][[1]]$WL)
        names<-as.character(Reduce(c, tib_spectra$Timestamp))


       clean_mat<- tibble(TS=names,
                          data.frame(t(abs_mat))) %>% drop_na
       names_clean<-clean_mat$TS
       clean_mat<-clean_mat %>% select(-TS) %>% as.matrix(.)
       unit<-c("wavelength","abs")
       colors=c(rep("black",times=length(names_clean)))
       groups=as.factor(c(rep("black",times=length(names_clean))))
       desc<-"Trios_spectra"
       sym<-c(rep(1,times=length(names_clean)))
       alt.sym=c(rep("a",times=length(names_clean)))

       # cal_dat=cal_dat_op[,2:7],)
       ###create ChemSpec object field abs
       my_op_spec<-list(freq=freq,
                        data=clean_mat,names=names_clean,unit=unit,colors=colors,groups=groups,desc=desc,sym=sym,alt.sym=alt.sym,dates=parse_date_time(names_clean,date_format))
       class(my_op_spec) <- "Spectra"
       list(spectra_tibble=tib_spectra,chemspectra=my_op_spec)
}

}

sumSpectra(test$chemspectra)

test<-import_spectra2(path = "D:/Uni work/Postdoc 2019-2020/Field work/India field_Patna-2019/Trios/All_Nov_trios data/DATA/OPUS",chemspec = F)

#plotSpectra(test$chemspectra,
       #     which = c(1, 2, 14, 1000),
        #    yrange = c(0, 5))
