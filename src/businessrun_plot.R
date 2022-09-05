# businessrun_plot.R | 2022-09-05
# für www.dataschmience.at | david selig

# Load & install libraries for this project
renv::restore()

# Save new/used libraries for reproducability
# renv::snapshot()

require("dplyr")
require("tidyverse")
require("formattable")
require("plotly")
require("scales")
require("RColorBrewer")
require("lubridate")
require("dataPreparation")
require("DT")
require("kableExtra")

# INFO
# Help for picking (colorblind safe) colors: https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=3

# Load scraped data -------------------------------------------------------

df_target <- readRDS("data/df_target.RDS")

# Data prepocessing -------------------------------------------------------

# CALCULATE AGE
df_target$age <- df_target$eventyear - df_target$birthyear
# IMPUTE DATA - replace values with median value (menschen unter 15, menschen über 100)
median_impute <- median(df_target[!(df_target$age < 15 | df_target$age > 85),]$age)
median(df_target[!(df_target$age < 15 | df_target$age > 85),]$age)
df_target[df_target$age < 15 | df_target$age > 85,]$age <- median_impute

# CAST/TRANSFORM "Gesamtzeit" (in seks_per_km und zur Visualisierung)
df_target$time <- sub(",", ".", df_target$time)
df_target <- within(df_target, time_splitted<-data.frame(do.call('rbind', strsplit(as.character(time), ':', fixed=TRUE))))
df_target$total_time_in_sec <- as.numeric(df_target$time_splitted$X1) * 3600 + as.numeric(df_target$time_splitted$X2) * 60 + as.numeric(df_target$time_splitted$X3)
df_target <- df_target %>% select(-time_splitted)
# transform to h:m:s for visualizations (trick from https://stackoverflow.com/questions/42413282/displaying-plotly-histogram-time-in-hhmmss)
R <- paste('1970-01-01', df_target$time)
df_target$total_time_transformed <- strptime(R, format="%Y-%m-%d %H:%M:%S")
df_target$total_time_transformed <- as.numeric(df_target$total_time_transformed) * 1000
# transform to pace (in mins)/kmh
df_target$pace_in_sec <- round(df_target$total_time_in_sec / 4100 * 1000,0) # secs per kilometre
# decided to round to full seconds for visualizations
df_target$pace_transformed <- sprintf('%02d:%02d:%02d', hour(seconds_to_period(df_target$pace_in_sec)), 
                                      minute(seconds_to_period(df_target$pace_in_sec)), 
                                      second(seconds_to_period(df_target$pace_in_sec)))
R <- paste('1970-01-01', df_target$pace_transformed)
df_target$pace_transformed <- strptime(R, format="%Y-%m-%d %H:%M:%S")
df_target$pace_transformed <- as.numeric(df_target$pace_transformed) * 1000

# REMOVE OUTLIERS FOR LATER
nrow(df_target) # [1] 106338
df_target_woOutliers <- dataPreparation::remove_sd_outlier(df_target, cols = "total_time_in_sec", n_sigmas = 5, verbose = TRUE)
nrow(df_target_woOutliers) # [1] 64 have been dropped. 106274 left

# Create charts -------------------------------------------------------

# PLACEHOLDER FOR ALL CHARTS
charts <- list()

# TeilnehmerInnen pro eventyear (insgesamt)
# TeilnehmerInnen pro gender, birthyear, eventyear

df <- df_target %>%
  group_by(eventyear, gender) %>%
  summarise(count = n()) %>%
  mutate(freq = round(count / sum(count)*100,1))

fig <- plot_ly(df, x = ~eventyear, y = ~count, color=~gender, 
               colors = brewer.pal(length(names(table(df$gender))),"RdYlBu"), 
               text=~freq, type = 'bar',
               hovertemplate = paste('%{y}',
                                     '<b>(%{text} %)</b>'))
fig <- fig %>% layout(title = 'Bar Chart | <b>Anzahl der TeilnehmerInnen</b>', 
                      xaxis = list(title = '<b>Jahr</b>'), 
                      yaxis = list(title = '<b>TeilnehmerInnen</b>'), 
                      barmode = 'stack')
fig <- fig %>%
  layout(hovermode = "x unified")
charts$barchart_anzahl_teilnehmerinnen <- fig

# boxplot
# average time
# average pace
fig <- plot_ly(df_target, x = ~eventyear, y = ~total_time_transformed, color = ~gender, 
               colors = brewer.pal(length(names(table(df$gender))),"RdYlBu"), type = "box") %>% 
  layout(yaxis=list(type="date", tickformat="%H:%M:%S"))
fig <- fig %>% layout(boxmode = "group")
fig <- fig %>% layout(title = 'Box Plot | <b>Gesamtzeit der TeilnehmerInnen</b>', 
                      xaxis = list(title = '<b>Jahr</b>'), 
                      yaxis = list(title = '<b>Gesamtzeit (4.1 km)</b>'), 
                      barmode = 'stack')
fig
charts$boxplot_gesamtzeit_teilnehmerinnen <- fig

# ACHTUNG: sample der Daten, weil der Plot etwas zu groß ist fürs Export nach https://chart-studio.plotly.com/create/#/ (für Blog)
df_sampled <- df_target %>% sample_frac(0.35, replace = F)

fig <- plot_ly(df_sampled, x = ~eventyear, y = ~total_time_transformed, color = ~gender, 
               colors = brewer.pal(length(names(table(df$gender))),"RdYlBu"), type = "box") %>% 
  layout(yaxis=list(type="date", tickformat="%H:%M:%S"))
fig <- fig %>% layout(boxmode = "group")
fig <- fig %>% layout(title = 'Box Plot | <b>Gesamtzeit der TeilnehmerInnen</b>', 
                      xaxis = list(title = '<b>Jahr</b>'), 
                      yaxis = list(title = '<b>Gesamtzeit (4.1 km)</b>'), 
                      barmode = 'stack')
fig
charts$boxplot_gesamtzeit_teilnehmerinnen <- fig

# histogram paces (dataset: removed outliers)
fig <- plot_ly(alpha = 0.6, colors = brewer.pal(length(names(table(df_target_woOutliers$gender))),"RdYlBu"))
fig <- fig %>% add_histogram(x = ~df_target_woOutliers[df_target_woOutliers$gender == "Male",]$pace_transformed, name ='Male', color='#fc8d59')
fig <- fig %>% add_histogram(x = ~df_target_woOutliers[df_target_woOutliers$gender == "Female",]$pace_transformed, name ='Female', color='#91bfdb')
fig <- fig %>% layout(barmode = "overlay",
                      colors=c('#fc8d59','#91bfdb'),
                      xaxis=list(type="date", tickformat="%H:%M:%S", title = '<b>Pace (min/km)</b>'),
                      title = 'Histogramm | <b>Pace (min/km) der TeilnehmerInnen</b>', 
                      yaxis = list(title = '<b>Anzahl TeilnehmerInnen</b>'))
fig
charts$histogram_pace_in_mins_woOutliers <- fig

# pace per gender & birthyear
fig <- plot_ly(
  df_target_woOutliers, x = ~pace_transformed, y = ~age,
  # Hover text:
  text = ~paste("Pace (min/km): ", pace_transformed, '<br>Alter:', age),
  color = ~age, size = ~pace_transformed,
  colors = "YlOrRd"
)
fig <- fig %>% layout(xaxis=list(type="date", tickformat="%H:%M:%S", title = '<b>Pace (min/km)</b>'),
                      title = 'Scatter Plot | <b>Pace (min/km) vs. Alter</b>', 
                      yaxis = list(title = '<b>Alter</b>'))
fig
charts$scatter_pace_in_mins_versus_age_woOutliers <- fig


# wieviele verschiedene länder haben teilgenommen, wieviele pro land 
df <- df_target_woOutliers %>%
  group_by(iaaf) %>%
  summarise(count = n()) %>%
  mutate(freq = round(count / sum(count)*100,1)) %>%
  arrange(-count)

charts$tbl_teilnehmerInnen_pro_land_woOutliers <- df

nrow(df) # 17 different countries
'
# A tibble: 176 × 3
   iaaf  count  freq
   <chr> <int> <dbl>
 1 AUT   94639  89.1
 2 NA     2943   2.8
 3 GER    1912   1.8
 4 HUN    1381   1.3
 5 SVK     605   0.6
 6 CZE     419   0.4
 7 POL     338   0.3
 8 ITA     304   0.3
 9 CRO     251   0.2
10 ROU     251   0.2
# … with 166 more rows
'

# welche firmen sind oft dabei
tbl_clubs <- df_target_woOutliers %>% group_by(club) %>% summarise(antreten=n())
# ganz zu unterst übrigens: "#meinchefzwingtmichdazu". auch super: "3 Muskateller", "A Team Has No Name", "6 Beine für ein Halleluja",

'
# TOP 10
unique(df_target_woOutliers[grepl("Bundesheer", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("KAV", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("Hofer KG", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("ÖBB", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("Robert Bosch AG", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("Wiener Stadtwerke", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("VAMED", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("Wirtschaftskammer Wien", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("UNIQA", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("younion", df_target_woOutliers$club, fixed = TRUE),]$club)

# ferner (persönliche auswahl)
unique(df_target_woOutliers[grepl("Österreichischer Fußball Bund", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("Casinos", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("Cashpoint", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("Novomatic", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("Greentube", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("bwin", df_target_woOutliers$club, fixed = TRUE),]$club)

unique(df_target_woOutliers[grepl("Interwetten", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("bet-at-home", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("win2day", df_target_woOutliers$club, fixed = TRUE),]$club)
unique(df_target_woOutliers[grepl("tipp3", df_target_woOutliers$club, fixed = TRUE),]$club)
'

# let's sum it up
# top10 companies (meisten teilnehmerInnen)
df_target_woOutliers_TOP10 <- df_target_woOutliers
df_target_woOutliers_TOP10$club_agg <- ""
df_target_woOutliers_TOP10[grepl("Bundesheer", df_target_woOutliers_TOP10$club, fixed = TRUE),]$club_agg <- "Österreichisches Bundesheer"
df_target_woOutliers_TOP10[grepl("KAV", df_target_woOutliers_TOP10$club, fixed = TRUE),]$club_agg <- "KAV"
df_target_woOutliers_TOP10[grepl("Hofer KG", df_target_woOutliers_TOP10$club, fixed = TRUE),]$club_agg <- "Hofer KG"
df_target_woOutliers_TOP10[grepl("ÖBB", df_target_woOutliers_TOP10$club, fixed = TRUE),]$club_agg <- "ÖBB"
df_target_woOutliers_TOP10[grepl("Robert Bosch AG", df_target_woOutliers_TOP10$club, fixed = TRUE),]$club_agg <- "Robert Bosch AG"
df_target_woOutliers_TOP10[grepl("Wiener Stadtwerke", df_target_woOutliers_TOP10$club, fixed = TRUE),]$club_agg <- "Wiener Stadtwerke"
df_target_woOutliers_TOP10[grepl("VAMED", df_target_woOutliers_TOP10$club, fixed = TRUE),]$club_agg <- "VAMED"
df_target_woOutliers_TOP10[grepl("Wirtschaftskammer Wien", df_target_woOutliers_TOP10$club, fixed = TRUE),]$club_agg <- "Wirtschaftskammer Wien"
df_target_woOutliers_TOP10[grepl("UNIQA", df_target_woOutliers_TOP10$club, fixed = TRUE),]$club_agg <- "UNIQA"
df_target_woOutliers_TOP10[grepl("younion", df_target_woOutliers_TOP10$club, fixed = TRUE),]$club_agg <- "younion"
df_target_woOutliers_TOP10 <- df_target_woOutliers_TOP10[df_target_woOutliers_TOP10$club_agg != "",]
nrow(df_target_woOutliers_TOP10)
# order by median
tbl_df_target_woOutliers_TOP10 <- df_target_woOutliers_TOP10 %>% group_by(club_agg) %>% summarise(median=median(total_time_in_sec)) %>% as.data.frame()
tbl_df_target_woOutliers_TOP10 <- tbl_df_target_woOutliers_TOP10[order(tbl_df_target_woOutliers_TOP10$median),]
df_target_woOutliers_TOP10$club_agg <- ordered(df_target_woOutliers_TOP10$club_agg, levels=c(tbl_df_target_woOutliers_TOP10$club_agg))

fig <- plot_ly(df_target_woOutliers_TOP10, x = ~club_agg, y = ~pace_transformed, #color = ~gender,
               type = "box") %>% 
  layout(yaxis=list(type="date", tickformat="%H:%M:%S"))
fig <- fig %>% layout(boxmode = "group")
fig <- fig %>% layout(title = 'Box Plot | <b>Pace (min/km) der TeilnehmerInnen</b> <br>(TOP10 Unternehmen mit meisten TeilnehmerInnen)', 
                      xaxis = list(title = '<b>Auswahl Unternehmen</b>'), 
                      yaxis = list(title = '<b>Gesamtzeit (4.1 km)</b>'), 
                      barmode = 'stack')
fig
charts$boxplot_pace_in_mins_top10_companies_woOutliers <- fig

# ein paar willkürlich selektierte Unternehmen
df_target_woOutliers_SELECTED <- df_target_woOutliers
df_target_woOutliers_SELECTED$club_agg <- ""
df_target_woOutliers_SELECTED[grepl("Österreichischer Fußball Bund", df_target_woOutliers_SELECTED$club, fixed = TRUE),]$club_agg <- "Österreichischer Fußball Bund"
df_target_woOutliers_SELECTED[grepl("Casinos", df_target_woOutliers_SELECTED$club, fixed = TRUE),]$club_agg <- "Casinos Austria"
df_target_woOutliers_SELECTED[grepl("Cashpoint", df_target_woOutliers_SELECTED$club, fixed = TRUE),]$club_agg <- "Cashpoint"
df_target_woOutliers_SELECTED[grepl("Novomatic", df_target_woOutliers_SELECTED$club, fixed = TRUE),]$club_agg <- "Novomatic"
df_target_woOutliers_SELECTED[grepl("Greentube", df_target_woOutliers_SELECTED$club, fixed = TRUE),]$club_agg <- "Greentube"
df_target_woOutliers_SELECTED[grepl("bwin", df_target_woOutliers_SELECTED$club, fixed = TRUE),]$club_agg <- "bwin"
df_target_woOutliers_SELECTED <- df_target_woOutliers_SELECTED[df_target_woOutliers_SELECTED$club_agg != "",]
nrow(df_target_woOutliers_SELECTED)
# order by median
tbl_df_target_woOutliers_SELECTED <- df_target_woOutliers_SELECTED %>% group_by(club_agg) %>% summarise(median=median(total_time_in_sec)) %>% as.data.frame()
tbl_df_target_woOutliers_SELECTED <- tbl_df_target_woOutliers_SELECTED[order(tbl_df_target_woOutliers_SELECTED$median),]
df_target_woOutliers_SELECTED$club_agg <- ordered(df_target_woOutliers_SELECTED$club_agg, levels=c(tbl_df_target_woOutliers_SELECTED$club_agg))

fig <- plot_ly(df_target_woOutliers_SELECTED, x = ~club_agg, y = ~pace_transformed, 
               type = "box") %>% 
  layout(yaxis=list(type="date", tickformat="%H:%M:%S"))
fig <- fig %>% layout(boxmode = "group")
fig <- fig %>% layout(title = 'Box Plot | <b>Pace (min/km) der TeilnehmerInnen</b> <br>(handselektiert vom Autor)', 
                      xaxis = list(title = '<b>Auswahl Unternehmen</b>'), 
                      yaxis = list(title = '<b>Gesamtzeit (4.1 km)</b>'), 
                      barmode = 'stack'
)
fig
charts$boxplot_pace_in_mins_selected_companies_woOutliers <- fig

# wo stehen die gambling-firmen denn insgesamt (annotations showcase)?
df_target_woOutliers_ANNOTATIONS <- df_target_woOutliers
df_target_woOutliers_ANNOTATIONS$club_agg <- ""
df_target_woOutliers_ANNOTATIONS[grepl("Casinos", df_target_woOutliers_ANNOTATIONS$club, fixed = TRUE),]$club_agg <- "Gambling Companies"
df_target_woOutliers_ANNOTATIONS[grepl("Cashpoint", df_target_woOutliers_ANNOTATIONS$club, fixed = TRUE),]$club_agg <- "Gambling Companies"
df_target_woOutliers_ANNOTATIONS[grepl("Novomatic", df_target_woOutliers_ANNOTATIONS$club, fixed = TRUE),]$club_agg <- "Gambling Companies"
df_target_woOutliers_ANNOTATIONS[grepl("Greentube", df_target_woOutliers_ANNOTATIONS$club, fixed = TRUE),]$club_agg <- "Gambling Companies"
df_target_woOutliers_ANNOTATIONS[grepl("bwin", df_target_woOutliers_ANNOTATIONS$club, fixed = TRUE),]$club_agg <- "Gambling Companies"
df_target_woOutliers_ANNOTATIONS[df_target_woOutliers_ANNOTATIONS$club_agg != "Gambling Companies", ]$club_agg <- "Other Companies"

# gambling companies vs nicht
fig <- plot_ly(df_target_woOutliers_ANNOTATIONS, x = ~club_agg, y = ~pace_transformed, #color = ~gender, 
               color= ~club_agg,
               colors = c('#d8b365','#5ab4ac'),
               type = "box") %>% 
  layout(yaxis=list(type="date", tickformat="%H:%M:%S"))
fig <- fig %>% layout(boxmode = "group")
fig <- fig %>% layout(title = 'Box Plot | <b>Pace (min/km) der TeilnehmerInnen</b> <br>(handselektiert vom Autor)', 
                      xaxis = list(title = '<b>Auswahl Unternehmen</b>'), 
                      yaxis = list(title = '<b>Gesamtzeit (4.1 km)</b>'), 
                      barmode = 'stack')
fig
charts$boxplot_pace_in_mins_gambling_companies_woOutliers <- fig

# wer sind die schnellsten unternehmen
tbl_df_target_woOutliers_TOP10 <- df_target_woOutliers %>% group_by(club) %>% summarise(occurences=n(), median=median(total_time_in_sec)) %>% as.data.frame()
tbl_df_target_woOutliers_TOP10 <- tbl_df_target_woOutliers_TOP10[order(tbl_df_target_woOutliers_TOP10$median),]
tbl_df_target_woOutliers_TOP10 <- tbl_df_target_woOutliers_TOP10[tbl_df_target_woOutliers_TOP10$occurences > 100,]
head(tbl_df_target_woOutliers_TOP10, n=25)

charts$tbl_pace_in_secs_all_companies_woOutliers <- tbl_df_target_woOutliers_TOP10

# table pace for top %
# für Männer
quantile(df_target_woOutliers[df_target_woOutliers$gender == "Male",]$pace_in_sec, probs=c(0.1,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50),na.rm=T)
quantile(df_target_woOutliers[df_target_woOutliers$gender == "Male",]$total_time_in_sec, probs=c(0.1,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50),na.rm=T)

# für Frauen
quantile(df_target_woOutliers[df_target_woOutliers$gender == "Female",]$pace_in_sec, probs=c(0.1,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50),na.rm=T)
quantile(df_target_woOutliers[df_target_woOutliers$gender == "Female",]$total_time_in_sec, probs=c(0.1,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50),na.rm=T)


# THIS IS WHERE THE MAGIC HAPPENS
# histogram for input train_run_tries
# wo landet man mit welcher wahrscheinlichkeit a) insgesamt b) in den einzelnen jahren

df <- df_target_woOutliers %>% select(c(gender, pace_in_sec, eventyear))

hlp_ecdf_male <- ecdf(df[which(df$gender == "Male"),]$pace_in_sec)
hlp_ecdf_female <- ecdf(df[which(df$gender == "Female"),]$pace_in_sec)
df$percentile <- ifelse(df$gender == "Male", hlp_ecdf_male(df$pace_in_sec), hlp_ecdf_female(df$pace_in_sec))

df[df$gender == "Male",]$gender <- "Männer"
df[df$gender == "Female",]$gender <- "Frauen"

fig <- df %>% 
  mutate(pace_in_sec.Cut = cut(pace_in_sec, breaks = seq(170, 835, 1), right = FALSE)) %>% 
  group_by(pace_in_sec.Cut, gender) %>% 
  summarise(n = n(), percentile = round(mean(percentile),5)) %>% 
  mutate(text=paste0("Mit einem <b>Pace</b> von etwa <b>", 
                     sprintf('%02d:%02d:%02d', hour(seconds_to_period(as.numeric(sub(",.*", "", substring(as.character(pace_in_sec.Cut),2))))), 
                             minute(seconds_to_period(as.numeric(sub(",.*", "", substring(as.character(pace_in_sec.Cut),2))))), 
                             second(seconds_to_period(as.numeric(sub(",.*", "", substring(as.character(pace_in_sec.Cut),2))))))
                     #sub(",.*", "", substring(as.character(pace_in_sec.Cut),2))
                     , " (h:m:s pro km)</b>",
                     #"s und ", gsub(')', '', sub('.*,\\s*', '', as.character(pace_in_sec.Cut))), "s",
                     "\nsollten Sie unter den <b>", gender, "-TOP ", percentile*100, "%</b> landen!"
  )) %>%
  plot_ly(x = ~pace_in_sec.Cut, y = ~n, text = ~text, color=~gender, alpha=0.99,
          colors = brewer.pal(length(names(table(df_target_woOutliers$gender))),"RdYlBu")) %>% 
  add_bars(marker = list(colorscale = "Rainbow"), hovertemplate = "%{y}<br>%{text}") %>%
  layout(hovermode = "x unified",
         title='Analyse | <b>Wo würden Sie beim Business Run landen?',
         xaxis=list(title = 'Pace (Sekunden/km)'),
         yaxis=list(title = 'Anzahl TeilnehmerInnen'))
fig
charts$histogram_lookup_pace_per_gender_woOutliers <- fig

# (interactive) boxplot
df <- df_target_woOutliers %>% select(c(gender, pace_in_sec, eventyear))
df$gender = as.factor(df$gender)

plottest <- plot_ly(data = df,
                    type = 'box', x= ~ eventyear, y= ~ pace_in_sec, name = "Gesamt", visible = TRUE, xaxis = "x", yaxis = "y") %>%
  add_trace(data = df[which(df$gender == "Male"),], 
            type = "box",  x = ~ eventyear, y = ~ pace_in_sec, 
            name = "Männer", xaxis = "x3", yaxis = "y3", visible = FALSE, inherit = FALSE) %>%
  add_trace(data = df[which(df$gender == "Female"),], 
            type = "box",  x = ~ eventyear, y = ~ pace_in_sec,
            name = "Frauen", xaxis = "x4", yaxis = "y4", visible = FALSE, inherit = FALSE)

fig <- layout(plottest, title = "Initial Title",
              xaxis = list(visible = TRUE), 
              xaxis3 = list(overlaying = "x", visible = FALSE), 
              xaxis4 = list(overlaying = "x", visible = FALSE),
              yaxis = list(visible = TRUE, title = "Pace (in sec)", range=c(0,900)),
              yaxis3 = list(overlaying = "y", visible = FALSE, title = "Pace (in sec)", range=c(0,900)),
              yaxis4 = list(overlaying = "y", visible = FALSE, title = "Pace (in sec)", range=c(0,900)),
              updatemenus = list(
                list(
                  y = 0.7,
                  buttons = list(
                    list(label = "Gesamt",
                         method = "update",
                         args = list(list(name = "Gesamt", visible = c(TRUE,FALSE,FALSE)),
                                     list(title = "title - Gesamt",
                                          xaxis = list(visible = TRUE),
                                          xaxis3 = list(overlaying = "x", visible = FALSE),
                                          xaxis4 = list(overlaying = "x", visible = FALSE),
                                          yaxis = list(visible = TRUE, title = "Pace (in sec)", range=c(0,900)),
                                          yaxis3 = list(overlaying = "y", visible = FALSE),
                                          yaxis4 = list(overlaying = "y", visible = FALSE)
                                     ))
                    ),
                    list(label = "Männer",
                         method = "update",
                         args = list(list(name = "Male", visible = c(FALSE,TRUE,FALSE)),
                                     list(title = "title - Male",
                                          xaxis = list(visible = FALSE),
                                          xaxis3 = list(overlaying = "x", visible = TRUE),
                                          xaxis4 = list(overlaying = "x", visible = FALSE),
                                          yaxis = list(visible = FALSE),
                                          yaxis3 = list(overlaying = "y", visible = TRUE, title = "Pace (in sec)", range=c(0,900)),
                                          yaxis4 = list(overlaying = "y", visible = FALSE)
                                     ))
                    ),
                    list(label = "Frauen",
                         method = "update",
                         args = list(list(name = "Female", visible = c(FALSE,FALSE,TRUE)),
                                     list(title = "title - Female",
                                          xaxis = list(visible = FALSE),
                                          xaxis3 = list(overlaying = "x", visible = FALSE),
                                          xaxis4 = list(overlaying = "x", visible = TRUE),
                                          yaxis = list(visible = FALSE),
                                          yaxis3 = list(overlaying = "y", visible = FALSE),
                                          yaxis4 = list(overlaying = "y", visible = TRUE, title = "Pace (in sec)", range=c(0,900))
                                     )))
                  ))))

fig
charts$boxplot_lookup_pace_per_gender_woOutliers <- fig


# Save charts -------------------------------------------------------------

# saveRDS(charts, "results/charts.RDS")

# Load charts -------------------------------------------------------------
charts <- readRDS("results/charts.RDS")
names(charts)

# TABLE TEILNEHMER/COUNTRIES
charts$tbl_teilnehmerInnen_pro_land_woOutliers$Position <- c(1:nrow(charts$tbl_teilnehmerInnen_pro_land_woOutliers))
tbl_countries <- charts$tbl_teilnehmerInnen_pro_land_woOutliers %>%
  replace(is.na(.), "-") %>%
  rename(Land = iaaf, Anzahl = count, Prozent = freq) %>%
  mutate(Prozent = Prozent/100) %>%
  #head(10) %>%
  select(Position, Land, Anzahl, Prozent)

ft_tbl_countries <- formattable(tbl_countries, 
            align = c("c",rep("c", NCOL(tbl_countries) - 1)),
            list(`Position` = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")), 
                 area(col = "Prozent") ~ function(x) percent(x, digits = 1),
                 area(col = c("Prozent")) ~ color_tile("#DeF7E9", "#71CA97"))) %>%
  as.datatable(escape = FALSE,
               options = list(scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets = 0:3))),
               rownames = FALSE
               #, filter = 'bottom'              ## include column filters at the bottom
  )
                 
# TABLE UNTERNEHMEN/PACE
charts$tbl_pace_in_secs_all_companies_woOutliers$Position <- c(1:nrow(charts$tbl_pace_in_secs_all_companies_woOutliers))
tbl_companies <- charts$tbl_pace_in_secs_all_companies_woOutliers %>%
  rename(Unternehmen = club, Anzahl = occurences, pace_median = median) %>%
  mutate("Pace (Median)" = sprintf('%02d:%02d:%02d', hour(seconds_to_period(as.integer(pace_median))), 
                                   minute(seconds_to_period(as.integer(pace_median))), 
                                   second(seconds_to_period(as.integer(pace_median))))) %>%
  #head(10) %>%
  select(Position, Unternehmen, Anzahl, "Pace (Median)")

row.names(tbl_companies) <- NULL

ft_tbl_companies <- formattable(tbl_companies, 
                                align = c("c",rep("c", NCOL(tbl_companies) - 1)),
                                list(`Position` = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")), 
                                     #area(col = "Prozent") ~ function(x) percent(x, digits = 1),
                                     area(col = c("Pace (Median)")) ~ color_tile("#DeF7E9", "#71CA97"))) %>%
  as.datatable(escape = FALSE,
               options = list(scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets = 0:3))),
               rownames = FALSE,
               server
              
               #, filter = 'bottom'              ## include column filters at the bottom
  )
ft_tbl_companies


charts$tbl_pace_in_secs_all_companies_woOutliers$Position <- c(1:nrow(charts$tbl_pace_in_secs_all_companies_woOutliers))
tbl_companies <- charts$tbl_pace_in_secs_all_companies_woOutliers %>%
  rename(Unternehmen = club, Anzahl = occurences, pace_median = median) %>%
  mutate(pace_median = pace_median / 4100 * 1000) %>%
  mutate("Pace (Median)" = sprintf('%02d:%02d:%02d', hour(seconds_to_period(as.integer(pace_median))), 
                                   minute(seconds_to_period(as.integer(pace_median))), 
                                   second(seconds_to_period(as.integer(pace_median))))) %>%
  #head(10) %>%
  select(Position, Unternehmen, Anzahl, "Pace (Median)")

row.names(tbl_companies) <- NULL

# as KBL (wordpress ...)
ft_dt <- tbl_companies
row.names(ft_dt) <- NULL
ft_dt$Position <- formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold"))(ft_dt$Position)
ft_dt$Anzahl <- color_tile("#DeF7E9", "#FF7F7F")(ft_dt$Anzahl)

kbl(ft_dt, escape = F, align=c('c','l','c','c','c')) %>%
  kable_paper("hover", full_width = T) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
          fixed_thead = T,position = "center") %>% #, html_font = "Cambria"
  column_spec(4, width = "3cm") %>%
  scroll_box(height = "500px") %>% 
  save_kable(file = "results/kbl_tbl_companies.html", self_contained = T)


charts$tbl_teilnehmerInnen_pro_land_woOutliers$Position <- c(1:nrow(charts$tbl_teilnehmerInnen_pro_land_woOutliers))
tbl_countries <- charts$tbl_teilnehmerInnen_pro_land_woOutliers %>%
  replace(is.na(.), "-") %>%
  rename(Land = iaaf, Anzahl = count, Prozent = freq) %>%
  mutate(Prozent = percent(Prozent/100,ndigits=1)) %>%
  #head(10) %>%
  select(Position, Land, Anzahl, Prozent)

# as KBL (wordpress ...)
ft_dt <- tbl_countries
row.names(ft_dt) <- NULL
ft_dt$Position <- formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold"))(ft_dt$Position)
ft_dt$Anzahl <- color_tile("#DeF7E9", "#FF7F7F")(ft_dt$Anzahl)

kbl(ft_dt, escape = F, align=c('c','l','c','c','c')) %>%
  kable_styling( #bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                fixed_thead = T,position = "center") %>% #, html_font = "Cambria"
  column_spec(4, width = "3cm") %>%
  scroll_box(height = "500px") %>% 
  save_kable(file = "results/kbl_tbl_countries2.html", self_contained = T)


# Überblick existierende Charts/Tables ------------------------------------

# Allgemeine Stats, gemeinsam mit Nationen_table
charts$barchart_anzahl_teilnehmerinnen
ft_tbl_countries

# Boxplot als Übersicht wegen Ausreißern und nach Geschlecht, auf filterbares verzichten (kein Mehrwert)
charts$boxplot_gesamtzeit_teilnehmerinnen
# charts$boxplot_lookup_pace_per_gender_woOutliers

# Alter, keine Aussagekraft, daher auch nicht genutzt
# charts$scatter_pace_in_mins_versus_age_woOutliers

# Boxplots der Unternehmen und dazu tables
charts$boxplot_pace_in_mins_top10_companies_woOutliers
charts$boxplot_pace_in_mins_selected_companies_woOutliers
# charts$boxplot_pace_in_mins_gambling_companies_woOutliers
ft_tbl_companies

# Histogramm Lookup, daher die interaktiven Boxplots ignoriert
#charts$histogram_pace_in_mins_woOutliers
charts$histogram_lookup_pace_per_gender_woOutliers

# Export Charts -----------------------------------------------------------

# EXPORT TO PLOTLY CHART STUDIO
Sys.setenv("plotly_username"="")
Sys.setenv("plotly_api_key"="")
#api_create(charts$histogram_lookup_pace_per_gender_woOutliers, filename = "business_run_histogram_lookup_pace_per_gender_woOutliers")
#api_create(charts$barchart_anzahl_teilnehmerinnen, filename = "business_run_barchart_anzahl_teilnehmerinnen")
# FAIL weil too big api_create(charts$boxplot_gesamtzeit_teilnehmerinnen, filename = "business_run_boxplot_gesamtzeit_teilnehmerinnen")
# daher sampled zu 35%
#api_create(charts$boxplot_gesamtzeit_teilnehmerinnen, filename = "business_run_boxplot_gesamtzeit_teilnehmerinnen")
#api_create(charts$boxplot_pace_in_mins_top10_companies_woOutliers, filename = "business_run_boxplot_pace_in_mins_top10_companies_woOutliers")
#api_create(charts$boxplot_pace_in_mins_selected_companies_woOutliers, filename = "business_run_boxplot_pace_in_mins_selected_companies_woOutliers")
# manueller export (als HTML in Wordpress kopieren)
###api_create(ft_tbl_countries, filename = "business_run_ft_tbl_countries")
###api_create(ft_tbl_companies, filename = "business_run_ft_tbl_companies")