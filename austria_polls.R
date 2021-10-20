## Load the libraries
library("tidyverse")
library("lubridate")

## Generate a tibble with election dates
election <- tibble(
  `firm` = c("Election 2019", "Election 2017"), 
  `ÖVP` = c(37.5, 31.5),
  `SPÖ` = c(21.2, 26.9),
  `FPÖ` = c(16.2, 26.0),
  `year` = c(2019, 2017), 
  `month` = c(9, 10),
  `day` = c(29, 15),
  `date` = c(ymd("2019/09/29"),ymd("2017/10/15")),
  `Institute`= c("Election","Election"))
  

## Clean the polling data. Dates are entered differently, firm names are pretty messy
df_polls <- read_csv("austria_polls.csv") %>% 
  filter(!str_detect(fieldwork, "September|Early")) %>% 
  separate(fieldwork, into = c("date1", "date2"), sep = "–") %>% 
  mutate(date1 = ifelse(!is.na(date2), NA, date1),
         date1 = as_date(date1),
         date2 = as_date(dmy(date2)),
         date = coalesce(date1,date2)) %>% 
  dplyr::select(-c(date1, date2)) %>% 
  mutate(firm = ifelse(str_detect(firm, "Affairs"), "Research Affairs", firm),
         firm = ifelse(firm =="Gallup/Östtereich", "Gallup/Östereich", firm),
         firm = ifelse(str_detect(firm, "AKonsult|Akonsult"), "AKonsult", firm),
         firm = ifelse(str_detect(firm, "Gallup"), "Gallup", firm),
         firm = ifelse(str_detect(firm, "GfK"), "GfK", firm),
         firm = ifelse(str_detect(firm, "Market|Marked"), "Market", firm),
         firm = ifelse(str_detect(firm, "IFES"), "IFES", firm),
         firm = ifelse(str_detect(firm, "Spectra"), "Spectra", firm),
         firm = ifelse(str_detect(firm, "OGM"), "OGM", firm),
         firm = ifelse(str_detect(firm, "IMAS"), "IMAS", firm),
         firm = ifelse(str_detect(firm, "Karmasin"), "Karmasin", firm),
         firm = ifelse(str_detect(firm, "Unique Research"), "Unique Research", firm),
         firm = ifelse(str_detect(firm, "meinungsraum"), "meinungsraum", firm),
         Institute = ifelse(firm == "Research Affairs", "Research Affairs", 
                            ifelse(firm == "Election", "Election", "Other"))) %>% 
  separate(date, into = c("year", "month", "day"), sep = "-", remove = FALSE) %>% 
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day)) %>% 
  bind_rows(election) %>% 
  group_by(firm) %>% 
  add_tally(name = "n_polls")

# Visualize the data
## scatterplot of all polls over time
## highlighting polls from Research Affairs
df_polls %>% 
  filter(n_polls > 5 | Institute == "Election") %>% 
  dplyr::select(firm, date, ÖVP, Institute) %>% 
  ggplot(aes(x = date, y = ÖVP, group = Institute, color = Institute, shape = Institute)) +
  geom_point(size = 3) + 
  scale_color_manual(values=c("red", "#999999", "#E69F00")) +
  geom_vline(xintercept = as.numeric(as.Date("2017-05-12")), color = "black", lwd = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2017-10-15")), color = "gray", lwd = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2019-09-29")), color = "gray", lwd = 1) + 
  theme_minimal() +
  labs(x = "Date", y = "ÖVP Polling", title = "ÖVP Polling Results: Research Affairs vs other firms", 
       caption = "Note: The black line indicates date on which Sebastian Kurs took over the ÖVP. The gray lines indicate national elections.") +
  theme(text = element_text(size = 20), legend.position="bottom")   

# scatterplot of all polls between 2016 and 2018
# highlighting polls from Research Affairs
df_polls %>% 
  filter(n_polls > 5 | Institute == "Election") %>% 
  filter(date > as.Date("2016-06-01") & date < as.Date("2018-07-01")) %>% 
  dplyr::select(firm, date, ÖVP, Institute) %>% 
  ggplot(aes(x = date, y = ÖVP, group = Institute, color = Institute, shape = Institute)) +
  geom_point(size = 3) + 
  scale_color_manual(values=c("red", "#999999", "#E69F00")) +
  geom_vline(xintercept = as.numeric(as.Date("2017-05-12")), color = "black", lwd = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2017-10-15")), color = "gray", lwd = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2019-09-29")), color = "black", lwd = 1) + 
  theme_minimal() +
  labs(x = "Date", y = "ÖVP Polling", title = "ÖVP Polling Results: Research Affairs vs other firms", 
       caption = "Note: The black line indicates date on which Sebastian Kurs took over the ÖVP. The gray line indicates the 2017 election.") +
  theme(text = element_text(size = 20), legend.position="bottom")   


# barplot of the number of polls per firm
df_polls %>% 
  filter(date > as.Date("2016-06-01") & date < as.Date("2019-09-23")) %>% 
  filter(!str_detect(firm, "election|Election")) %>% 
  ggplot(aes(x = firm, fill = Institute)) +
  geom_bar(show.legend = FALSE) + theme_minimal() +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  labs(y = "Count", x = "Firm", title = "Number of polls by firm between June 2016 and September 2019") +
  theme(text = element_text(size = 20))   

 

