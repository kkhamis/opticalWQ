
#' Title
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @param path
#' @param date_format
#' @param chemspec
#'
#' @return
#' @export
#'
#' @examples
import_spectra<-function(path,date_format="ymd HMS",chemspec=F){
        ###----initial data extraction------------------------
        files <- dir(path = path ,recursive = T,
                     pattern="ABSORPTI.DAT",full.names = T)
        spectra<-list()##storage list
        data<- files %>% purrr::map(~readr::read_lines(.x,
                                                       skip_empty_rows = T))
        dates<-data %>% purrr::map(~stringr::str_subset(.x,"Date"))
        TS<-purrr::map(dates,~lubridate::parse_date_time(stringr::str_sub(.x,
                start = 12,end = 30),date_format))
        dat_s<-data %>% purrr::map(~stringr::str_which(.x, "Unit4")+4)
        dat_e<-data %>% purrr::map(~stringr::str_which(.x, "Unit4")+206)
        ls1<-list()##storage list1
        ls2<-list()##storage list2
        ###---------------main loop----------------------------
                for(i in 1:length(dat_s)){
                vec_l<-length(dat_s[[i]])
                ls1<-list()
                        for(j in 1:vec_l){
                 test_dat<-data[[i]][dat_s[[i]][j]:dat_e[[i]][j]]
        m<-strsplit(test_dat, " " ) %>%
                purrr::map(as.numeric) %>%  purrr::reduce(rbind)
                        mat<-tibble::as_tibble(m)
                        names(mat)<-c("WL","ABS","a","log_a")
                        mat<- mat %>% dplyr::mutate(a=(2.303*ABS/0.01),
                        log_a=log((2.303*ABS)/0.01))
                        ls1[[j]]<-mat
                        }
                ls2[[i]]<-ls1
                }
        ###------------------------create tibble---------#####
tib_spectra<-tibble::tibble(file=stringr::str_sub(files,start = 18,end=25),
                    Timestamp=TS,abs_data=ls2)


###------------------------create chemspec object---------#####
if(chemspec==F){
 list(spectra_tibble=tib_spectra)
} else {
        n<-length(tib_spectra$abs_data)
        for(i in 1:n){
        test_ls[[i]]<-purrr::map(tib_spectra$abs_data[[i]],
                                 ~purrr::pluck(.x,"ABS") )
                test_ls[[i]]<-test_ls[[i]] %>% purrr::reduce(cbind)
        }

        abs_mat<-test_ls[[1]]
        for(i in 2:n){
                abs_mat<-cbind(abs_mat,test_ls[[i]])
        }
        freq<-as.numeric(tib_spectra$abs_data[[1]][[1]]$WL)
        names<-as.character(purrr::Reduce(c, tib_spectra$Timestamp))


       clean_mat<- tibble::tibble(TS=names,
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

#sumSpectra(test$chemspectra)

#test<-import_spectra(path = "D:/Uni work/Postdoc 2019-2020/Field work/India field_Patna-2019/Trios/All_Nov_trios data/DATA/OPUS",chemspec = F)

#plotSpectra(test$chemspectra,
#which = c(1, 2, 14, 1000), yrange = c(0, 5))
