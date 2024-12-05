library(dplyr)
library(lubridate)
library(stringi)
library(ggplot2)
library(ggpubr)

path='./texas_bats_v3_screened_with_counts'
file.ls <- list.files(path, pattern="KEWX")

path2='./texas_bats_v3_long_with_counts'
file.ls2 <- list.files(path2, pattern="KEWX")

plot_list = list()

# compare screened and unscreened data
for (f in 1:length(file.ls)){
  
  screened = read.csv(file.path(path, file.ls[f])) %>% 
    filter(label=="swallow-roost") %>% 
    mutate(track_mean = mean(na.omit(number_of_bats_polar)),
           local_date = ymd(local_date)) %>% 
    group_by(local_date) %>% 
    summarise(daily_sum = sum(track_mean)) %>% 
    mutate(cummulative = cumsum(daily_sum), year = year(local_date))
  
  
  unscreened = read.csv(file.path(path, file.ls[f])) %>% 
    filter(original_label=="swallow-roost")%>% 
    mutate(track_mean = mean(na.omit(number_of_bats_polar)),
           local_date = ymd(local_date)) %>% 
    group_by(local_date) %>% 
    summarise(daily_sum = sum(track_mean)) %>% 
    mutate(cummulative = cumsum(daily_sum), year = year(local_date))
  
  plot_list[[f]] = ggplot() + 
    geom_step(aes(x=local_date, y=cummulative), data = screened, color="blue", size = 1)+
    geom_step(aes(x=local_date, y=cummulative), data = unscreened, color="red", size = 1)+
    scale_x_date(breaks = "3 month", date_labels = "%b")+
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 2000000000, by=500000000), limits = c(0, 2200000000))+
    theme_bw(base_size = 15)+
    ggtitle(screened$year[1])
  
  
}

ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
          plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
          ncol = 4, nrow = 2)


#compare long and short raw detection numbers
stationyear = stri_sub(list.files(path), 14, 22) 
plot_list = list()
i = 1

for (f in stationyear){

  short.raw = read.csv(file.path(path, list.files(path, pattern = f))) %>% 
    filter(original_label=="swallow-roost")%>% 
    mutate(track_mean = mean(na.omit(number_of_bats_polar)),
           local_date = ymd(local_date)) %>% 
    group_by(local_date) %>% 
    summarise(daily_sum = sum(track_mean)) %>% 
    mutate(cummulative = cumsum(daily_sum), year = year(local_date))
  
  long.raw = read.table(file.path(path2, list.files(path2, pattern = f)), 
                        header = TRUE, sep = ",") %>% 
    mutate(track_id = paste0(stri_sub(filename, 0, 12), "-", track_id)) %>% 
    group_by(track_id) %>% 
    mutate(ave_score_track = mean(det_score),
           highest_score = max(det_score)) %>% 
    filter(ave_score_track > 0.15 & highest_score > 0.5) %>% 
    mutate(track_mean = mean(na.omit(n_animals)),
           local_date = ymd(stri_sub(local_time, 0, 8))) %>% 
    group_by(local_date) %>% 
    summarise(daily_sum = sum(track_mean)) %>% 
    mutate(cummulative = cumsum(daily_sum), year = year(local_date))
  
  plot_list[[i]] = ggplot() + 
    geom_step(aes(x=local_date, y=cummulative), data = short.raw, color="blue", size = 1)+
    geom_step(aes(x=local_date, y=cummulative), data = long.raw, color="red", size = 1)+
    scale_x_date(breaks = "3 month", date_labels = "%b")+
    #scale_y_continuous(labels = scales::comma, breaks = seq(0, 2000000000, by=500000000), limits = c(0, 2200000000))+
    theme_bw(base_size = 15)+
    ggtitle(f)
  
  i = i+1
}

library(gridExtra)

pdf("plots.pdf", onefile = TRUE)
for (i in plot_list) {
  print(i)
}
dev.off()
