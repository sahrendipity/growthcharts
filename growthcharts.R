# growthcharts.R ----
library(lubridate)
library(openxlsx)
library(ggthemes)
library(childsds)

data_path <- setwd('..')

# data sources ----
# get weight for age dataset from https://www.who.int/childgrowth/standards/tab_wfa_girls_p_0_5.txt
wfa_girls <- read.xlsx("https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-age/tab_wfa_girls_p_0_5.xlsx",1)

# get my baby's data and min boundaries -----
# wfa_baby <- read.csv(file.path(data_path,"wfa_baby.csv"))
babyname <- wfa_baby$babyname[1]

birth_details <- wfa_baby %>% arrange(measure_date) %>% top_n(-1, measure_date)
current_details <- wfa_baby %>% arrange(measure_date) %>% top_n(1, measure_date)

birth_percentile <-  round(sds(value=birth_details$weight, sex="female", age=0, item="weight", ref=who.ref, type="perc")*100,2)
current_percentile <-  round(sds(value=current_details$weight, sex="female", age=interval(birth_details$measure_date, current_details$measure_date) %>% as.numeric('years'), item="weight", ref=who.ref, type="perc")*100,2)

# data transformation ------
#rescale WHO data to my baby
wfa_girls_scaled <- wfa_girls %>% 
  mutate(measure_date = birth_details$measure_date + months(Month)) %>% 
  filter(measure_date <= current_details$measure_date+months(1)) 

#plot -----
ggplot(wfa_girls_scaled, aes(x = measure_date)) +
  geom_line(aes(y=P3), colour="lightcoral") +
  geom_line(aes(y=P15), colour="darkorange1") +
  geom_line(aes(y=P50), colour="seagreen") +
  geom_line(aes(y=P85), colour="darkorange1") +
  geom_line(aes(y=P97), colour="lightcoral") +
  geom_line(data=wfa_baby, aes(y= weight), size=0.75, colour="black") +
  geom_point(data=wfa_baby, aes(y= weight), size=3, colour="black") +
  theme_bw() +
  labs(x=NULL,y="Weight (Kg)",title=paste0(babyname,"'s weight chart")) +
  annotate(geom="text", x=current_details$measure_date-weeks(2), y=3.1, label=paste0(birth_percentile,"th percentile at birth")) +
  annotate(geom="text", x=current_details$measure_date-weeks(2), y=2.9, label=paste0(current_percentile,"th percentile currently"))

