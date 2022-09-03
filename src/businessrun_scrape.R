

# INFO
# https://www.hdsports.at/laufen/wien-energie-business-run
# example URL https://pentek-timing.at/results.html?pnr=14123&cnr=1
#
# obviously, 14123 is "run id" and cnr=1 is simply results of businessrun (there#s also Teamwertung, Nordic-Walking, Betriebsratswertung etc)
# 14123 = resuls 2021
# - = COVID (2020)
# 13558 = 2019
# 13417 = 2018
# 13233 = 2017
#
# inspect - save as bash(cmd) - https://curlconverter.com/r

require("httr")
require("jsonlite")
require("dplyr")
require("tidyverse")

# Save used libraries for reproducability
# renv::snapshot()
# renv::restore()

set_config(config(ssl_verifypeer = 0L)) # disable SSl verification

# EXAMPLE CALL
cookies = c(
  'PHPSESSID' = '4uvb8nohg06qcm8ib5vlodb135',
  'cookieconsent_dismissed' = 'yes'
)

headers = c(
  `Accept` = '*/*',
  `Accept-Language` = 'de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7,fr;q=0.6',
  `Connection` = 'keep-alive',
  `Referer` = 'https://pentek-timing.at/results.html?pnr=14123&cnr=1',
  `Sec-Fetch-Dest` = 'empty',
  `Sec-Fetch-Mode` = 'cors',
  `Sec-Fetch-Site` = 'same-origin',
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36',
  `X-Requested-With` = 'XMLHttpRequest',
  `sec-ch-ua` = '"Google Chrome";v="105", "Not)A;Brand";v="8", "Chromium";v="105"',
  `sec-ch-ua-mobile` = '?0',
  `sec-ch-ua-platform` = '"Windows"'
)

params = list(
  `pnr` = '14123',
  `cnr` = '1',
  `from` = '0',
  `count` = '49'
)

res <- httr::GET(url = 'https://pentek-timing.at/ws-results/rs/getResults', httr::add_headers(.headers=headers), query = params, httr::set_cookies(.cookies = cookies))

# CREATE SOURCE TABLE FOR ALL BUSINESS RUNS
df_source <- data.frame(year=c(2017,2018,2019,2021), url=c(13233,13417,13558,14123))

# CREATE TARGET TABLE FOR ALL BUSINESS RUNS
df_target <- data.frame()

# LOOP THROUGH LIST OF BUSINESS RUNS
for (i in 1:nrow(df_source)) {
  vi_pnr <- df_source[i,]$url # set params.pnr to a specific business run
  vi_eventyear <- df_source[i,]$year
# KEEP COOKIES & HEADER, BUT ADAPT PARAMS ON EACH CALL (batches of 50)
  df_business_run <- data.frame()
  vi_from <- 0
  repeat {
    params = list(
      `pnr` = vi_pnr,
      `cnr` = '1',
      `from` = vi_from,
      `count` = '50'
    )
    
    # if curl response throws an error, we probably have to wait a bit (we're sorry)
    tmp_success <- FALSE
    while(!tmp_success) {
      res_try <- tryCatch({
        res <- httr::GET(url = 'https://pentek-timing.at/ws-results/rs/getResults', httr::add_headers(.headers=headers), query = params, httr::set_cookies(.cookies = cookies))
        tmp_success <- TRUE
      },
      error = function(e) {Sys.sleep(60)})
    }

    jsonRespText<-content(res,as="text", encoding = "UTF-8") 
    df_info <- fromJSON(jsonRespText)$Results
    
    if (length(df_info) == 0){
      break
    }
    
    df_participants <- df_info$Participants %>%
      map_df(as_tibble) %>% 
      select(-bib)
    
    df_batch <- cbind(df_participants, df_info) %>% select(-Participants)

    df_business_run <- bind_rows(df_batch, df_business_run)  
    # add information
    df_business_run$pnr <- vi_pnr
    df_business_run$eventyear <- vi_eventyear
    
    cat("\nFound more participants for ", vi_eventyear, ", df_business_run now has ", nrow(df_business_run), " entries.")
    
    vi_from <- vi_from+50
  }
  # HINT: bind_rows von dplyr:: (statt rbind.fill von plyr) bzw. statt rbind (unterschiedliche anzahl cols, zB weil jeder x-te auf einmal 2 Spalten mehr hat f?r members link o?)
  df_target <- bind_rows(df_business_run, df_target)
} 

# finally, save our table
# write.csv(df_target, "df_target.csv")
# saveRDS(df_target, "data/df_target.RDS") # preferrably (smaller/faster)



