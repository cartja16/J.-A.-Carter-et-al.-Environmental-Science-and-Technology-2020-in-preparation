# Load in packages ====

library(tidyverse)
library(RSelenium)
library(robotstxt)
library(rvest)
library(lubridate)
library(ggbeeswarm)

library(lattice)
library(latticeExtra)
library(extrafont)
library(relayer)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(viridis)

mySettings <- trellis.par.get()

windowsFonts(Times=windowsFont("TT Times New Roman"))
par(family = "Times")

# read in the PWS identified from the EPA SDWIS

all_pws <- read_csv(file = "G:/My Drive/WFU/Water samples/Lead and Copper Survey/SDWIS_data/water_system_details_school.childcare_all/all_pws.csv")

# Open remote web driver ====

remDr <- remoteDriver(remoteServerAddr = "###.###.##.###",
                      browser = "chrome",
                      port = #####)
)
remDr$open()

# needs to be initiated outside the loop

school_pws.id <- all_pws %>%
  dplyr::distinct() %>% # should be already distinct
  dplyr::pull()

ind.loop_out <- list()
lcr_samples_out <- NULL

# Scrape loop ====

for (i in seq_along(school_pws.id)) {
  
  # navigate to source query
  
  remDr$navigate("https://www.pwss.enr.state.nc.us/NCDWW2/")
  
  remDr$findElement(using = "css", "#number")
  remDr$mouseMoveToLocation(webElement = remDr$findElement(using = "css", "#number"))
  remDr$click()
  remDr$sendKeysToActiveElement(list(school_pws.id[i])) # enter unique PWS ID
  remDr$mouseMoveToLocation(webElement = remDr$findElement(using = "css", "#SampleType"))
  remDr$click()
  remDr$sendKeysToActiveElement(list(key = "down_arrow"))
  remDr$sendKeysToActiveElement(list(key = "down_arrow"))
  remDr$sendKeysToActiveElement(list(key = "down_arrow"))
  remDr$sendKeysToActiveElement(list(key = "down_arrow"))
  remDr$sendKeysToActiveElement(list(key = "down_arrow"))
  remDr$sendKeysToActiveElement(list(key = "down_arrow"))
  remDr$sendKeysToActiveElement(list(key = "down_arrow"))
  remDr$sendKeysToActiveElement(list(key = "enter"))
  remDr$mouseMoveToLocation(webElement = remDr$findElement(using = "css", "#SVFD"))
  remDr$doubleclick()
  remDr$sendKeysToActiveElement(list(key = "delete"))
  remDr$doubleclick()
  remDr$sendKeysToActiveElement(list(key = "delete"))
  remDr$doubleclick()
  remDr$sendKeysToActiveElement(list(key = "delete"))
  remDr$doubleclick()
  remDr$sendKeysToActiveElement(list(key = "delete"))
  remDr$doubleclick()
  remDr$sendKeysToActiveElement(list(key = "delete"))
  remDr$sendKeysToActiveElement(list("1/1/1900"))
  remDr$mouseMoveToLocation(webElement = remDr$findElement(using = "css", "#SVTD"))
  remDr$doubleclick()
  remDr$sendKeysToActiveElement(list(key = "delete"))
  remDr$doubleclick()
  remDr$sendKeysToActiveElement(list(key = "delete"))
  remDr$doubleclick()
  remDr$sendKeysToActiveElement(list(key = "delete"))
  remDr$doubleclick()
  remDr$sendKeysToActiveElement(list(key = "delete"))
  remDr$doubleclick()
  remDr$sendKeysToActiveElement(list(key = "delete"))
  remDr$sendKeysToActiveElement(list("5/14/2019"))
  remDr$mouseMoveToLocation(webElement = remDr$findElement(using = "css", "input:nth-child(2)"))
  remDr$click()
  
  # here is the output for all non-coliform analytes
  
  samples_html <- read_html(remDr$getPageSource()[[1]]) %>%
    html_table(fill = TRUE)
  
  samples_tib <- as_tibble(samples_html[[2]])
  
  samples_col.names <- samples_tib[1,]
  colnames(samples_tib) <- samples_col.names
  samples_tib <- dplyr::slice(samples_tib, -1)
  
  row.num <- which(samples_tib$`Analyte Name` == "LEAD")
  
  if (!purrr::is_empty(row.num)) { # only executes if it sees an entry for lead
    
    row.num <- row.num + 1 # because we sliced the first row off
    
    css.lead <- paste("tr:nth-child(", row.num, ") a", sep = "")
    
    remDr$mouseMoveToLocation(webElement = remDr$findElement(using = "css", css.lead))
    remDr$click() # we click to get Lead which includes Copper
    
    # Here are all of the LCR samples
    
    lcr_html <- read_html(remDr$getPageSource()[[1]]) %>%
      html_table(fill = TRUE)
    
    lcr_tib <- as_tibble(lcr_html[[2]])
    
    lcr_col.names <- lcr_tib[1,]
    colnames(lcr_tib) <- lcr_col.names
    lcr_tib <- dplyr::slice(lcr_tib, -1)
    
    for (j in seq_along(1:nrow(lcr_tib))) {
      
      css.lcr_ind <- paste("#AutoNumber8 tr:nth-child(", j + 1, ") a font")
      
      remDr$mouseMoveToLocation(webElement = remDr$findElement(using = "css", css.lcr_ind)) # we accesss an individual LCR sample and add because we took away a row
      remDr$click()
      
      lcr.ind_html <- read_html(remDr$getPageSource()[[1]]) %>%
        html_table(fill = TRUE)
      
      lcr.ind_tib <- as_tibble(lcr.ind_html[[4]])
      
      lcr.ind_col.names <- lcr.ind_tib[1,]
      colnames(lcr.ind_tib) <- lcr.ind_col.names
      lcr.ind_tib <- dplyr::slice(lcr.ind_tib, -1)
      
      ind.loop_out[[j]] <- lcr.ind_tib
      print(ind.loop_out[[j]])
      remDr$goBack()
    }
    
    # tibbles of interest for us
    
    # lcr tib; ind.loop_out
    
    # ind.loop_out_info cbinds lcr_tib with the actual sample values from ind.loop_out
    # lcr_samples is the mapped list to df
    
    ind.loop_out_info <- lapply(seq_along(ind.loop_out), function(x) cbind(ind.loop_out[[x]], lcr_tib[x,]))
    lcr_samples <- dplyr::bind_rows(ind.loop_out_info, .id = "column_label")
    lcr_samples$PWS.ID <- school_pws.id[i]
    
    lcr_samples_out <- rbind(lcr_samples_out, lcr_samples)
    
    print(lcr_samples_out)
  }
}

# scraped 05/15/19

# write.csv(lcr_samples_out, file = "NC_school.childcare_pws_scrape.csv", row.names = FALSE)

# load in data from scrape

# Data wrangling ====

setwd("G:/My Drive/WFU/Water samples/Lead and Copper Survey/SDWIS_data/water_system_details_school.childcare_all")

lcr_samples_out <- read_csv(file = "NC_school.childcare_pws_scrape.csv") # the raw results from the scrape

# clean data - remove NA's from rbind of duplicate html table, remove non-numeric characters, select matching units, etc

data.clean <- lcr_samples_out %>%
  dplyr::filter(!is.na(Type)) %>% # this rids the duplicate binidng from the scrape
  dplyr::mutate(
    units.CL = case_when(
      grepl("UG/L", `Concentration level`) ~ "ppb",
      grepl("MG/L", `Concentration level`) ~ "ppm",
      !grepl("UG/L|MG/l", `Concentration level`) ~ "not ppm or ppb"
    ),
    units.RL = case_when(
      grepl("UG/L", `Reporting Level`) ~ "ppb",
      grepl("MG/L", `Reporting Level`) ~ "ppm",
      !grepl("UG/L|MG/L", `Reporting Level`) ~ "not ppm or ppb"
    )) %>%
  dplyr::mutate(`Concentration level` = gsub("[^0-9\\.]", "", `Concentration level`)) %>%
  dplyr::mutate(`Reporting Level` = gsub("[^0-9\\.]", "", `Reporting Level`)) %>%
  dplyr::mutate(concentration.impute = as.numeric(`Concentration level`)) %>%
  dplyr::mutate(concentration.impute = case_when(is.na(concentration.impute) ~ as.numeric(`Reporting Level`),
                                                 !is.na(concentration.impute) ~ concentration.impute),
                `Collection Date` = mdy(`Collection Date`)) %>%
  dplyr::mutate(collection.year = year(`Collection Date`)) %>%
  dplyr::mutate(
    units = case_when(
      units.CL == "ppm" | units.RL == "ppm" ~ "ppm",
      units.CL == "ppb" | units.RL == "ppb" ~ "ppb",
      !units.CL %in% c("ppm", "ppb") | !units.RL %in% c("ppm", "ppb") ~ "no units"
    )
  ) %>%
  dplyr::filter(units != "no units") %>%
  dplyr::mutate(
    concentration.impute = case_when(
      units == "ppm" ~ concentration.impute,
      units == "ppb" ~ concentration.impute/1000
    ),
    `Reporting Level` = case_when(
      units == "ppm" ~ concentration.impute,
      units == "ppb" ~ concentration.impute/1000
    )) %>%
  dplyr::select(-contains("units")) %>%
  dplyr::select(-column_label) %>%
  dplyr::distinct()

# Read in accompanying data and add information to form a final "lead" tibble ====

school_facilities <- read_csv(file = "NC_school.childcare_water.system.details.csv") %>% # this comes from the "Water system details from the EPA site"
  #dplyr::filter(`PWS ID` %in% data.clean$PWS.ID) %>%
  dplyr::mutate(`First Reported Date` = dmy(`First Reported Date`),
                `Last Reported Date` = dmy(`Last Reported Date`)) # one date fails to parse

pws.id_206 <- data.clean %>% # the unique ID's from the NCDEQ scrape
  dplyr::select(PWS.ID) %>%
  dplyr::distinct() 

start.date <- school_facilities %>%
  dplyr::select(`PWS ID`, `First Reported Date`) %>%
  dplyr::distinct()

# start date scrape from NCDWW database

nc.deq_activity.start <- read_csv(file = "ncdeq_scrape_activity.start.csv")

data.clean$epa.start.year <- NA

data.clean$epa.start.year <- sapply(seq_along(data.clean$PWS.ID), function(x) year(start.date$`First Reported Date`[start.date$`PWS ID` == data.clean$PWS.ID[x]]))

data.clean$nc.deq.activity.start <- NA

data.clean$nc.deq.activity.start <- sapply(seq_along(data.clean$PWS.ID), function(x) year(nc.deq_activity.start$activity.start.date[nc.deq_activity.start$PWS.ID == data.clean$PWS.ID[x]]))

data.clean <- data.clean %>%
  dplyr::mutate(start.year = case_when(
    nc.deq.activity.start <= epa.start.year ~ nc.deq.activity.start,
    nc.deq.activity.start > epa.start.year ~ epa.start.year
  )) %>%
  dplyr::mutate(
    start.91 = case_when(
      start.year < 1991 ~ "Before",
      !start.year < 1991 ~ "After"
    ),
    start.87 = case_when(
      start.year < 1987 ~ "Before",
      !start.year < 1987 ~ "After"
    ),
    start.2014 = case_when(
      start.year < 2014 ~ "Before",
      !start.year < 2014 ~ "After"
    ))

# of the 1083, 206 report data to the NC Safe Drinking Water Watch

pws.id_in.ncdeq <- school_facilities %>%
  dplyr::filter(`PWS ID` %in% data.clean$PWS.ID) %>%
  dplyr::arrange(`PWS ID`)

pws.206.population.summary <- pws.id_in.ncdeq %>%
  dplyr::select(-column_label) %>%
  dplyr::group_by(`PWS ID`, `Pop Cat 5`) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::distinct(`PWS ID`, `Pop Cat 5`) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(`PWS ID`) %>%
  dplyr::mutate(n = n())

pws.id_not.in.ncdeq <- school_facilities %>%
  dplyr::filter(!`PWS ID` %in% data.clean$PWS.ID) %>%
  dplyr::arrange(`PWS ID`) 

# Read in NCDEQ LCR survey information

lcr_survey <- read_csv("G:/My Drive/WFU/Water samples/Lead and Copper Survey/Lead_and_Copper_Survey_Data.csv") # from nc.deq

# Read in facility information from EPA SDWIS

school_facilities_with.corrosion <- read_csv(file = "epa_facilities.csv") %>% # this comes from the facilities option on the epa site
  dplyr::select(-column_label) %>%
  dplyr::distinct()

activity_summary <- school_facilities_with.corrosion %>%
  dplyr::filter(`PWS ID` %in% pws.id_206$PWS.ID) %>%
  dplyr::distinct() %>%
  dplyr::group_by(`PWS ID`, `Activity Status`, `Facility Name`, `Facility Deactivation Date`) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::arrange(`PWS ID`, `Activity Status`)

facility_deactivate <- school_facilities_with.corrosion %>%
  dplyr::filter(`PWS ID` %in% pws.id_206$PWS.ID) %>%
  dplyr::filter(`Deactivation Date` != "-" | `Facility Deactivation Date` != "-") %>%
  dplyr::arrange(`PWS ID`)

pws.id_in.ncdeq.scrape_epa.corrosion <- school_facilities_with.corrosion %>%
  dplyr::filter(`PWS ID` %in% pws.id_206$PWS.ID)

pws.id_reported.corrosion.inhib <- pws.id_in.ncdeq.scrape_epa.corrosion %>%
  dplyr::filter(grepl("corrosion|Corrosion", `Treatment Objective`)) %>%
  dplyr::arrange(`PWS ID`)

# how many of the pws id's report corrosion inhibition in the EPA SDWIS?

nrow(pws.id_reported.corrosion.inhib %>%
  dplyr::select(`PWS ID`) %>%
  dplyr::distinct())

# the summary of what they use as found in the EPA data

pws.id_reported.corrosion.inhib_summary <- pws.id_reported.corrosion.inhib %>%
  dplyr::group_by(`PWS ID`, `Treatment Process`) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(`PWS ID`)

# Continue to build the final "lead" tibble for analysis by combining information from NCDEQ and EPA ====

lcr_survey_filt <- lcr_survey %>%
  dplyr::filter(`Water System ID (pwsid)` %in% data.clean$PWS.ID)

data.clean_survey <- data.clean %>%
  dplyr::filter(PWS.ID %in% lcr_survey_filt$`Water System ID (pwsid)`)

data.clean$leadsolder <- NA
data.clean$leadservice <- NA
data.clean$cus_goose <- NA
data.clean$leadgooseneck <- NA
data.clean$sys_goose <- NA
data.clean$leadpipe <- NA
data.clean$date.form.comp <- NA
data.clean$corrosion_control.nc.deq <- NA
data.clean$galvanized <- NA
data.clean$copper.mat <- NA
data.clean$brass <- NA
data.clean$nc.deq.survey.86 <- NA

data.clean$leadsolder <- sapply(seq_along(data.clean$PWS.ID), function (x) ifelse(data.clean$PWS.ID[x] %in% lcr_survey$`Water System ID (pwsid)`, lcr_survey_filt$`Lead from solder, caulking, alloys and home plumbing (leadsolder)`[lcr_survey_filt$`Water System ID (pwsid)` == data.clean$PWS.ID[x]], 2))
data.clean$leadservice <- sapply(seq_along(data.clean$PWS.ID), function (x) ifelse(data.clean$PWS.ID[x] %in% lcr_survey$`Water System ID (pwsid)`, lcr_survey_filt$`Lead service lines (water main to meter and/or meter to customer’s building) (leadservice)`[lcr_survey_filt$`Water System ID (pwsid)` == data.clean$PWS.ID[x]], 2))
data.clean$cus_goose <- sapply(seq_along(data.clean$PWS.ID), function (x) ifelse(data.clean$PWS.ID[x] %in% lcr_survey$`Water System ID (pwsid)`, lcr_survey_filt$`Lead goosenecks/pigtails (cus_goose)`[lcr_survey_filt$`Water System ID (pwsid)` == data.clean$PWS.ID[x]], 2))
data.clean$leadgooseneck <- sapply(seq_along(data.clean$PWS.ID), function (x) ifelse(data.clean$PWS.ID[x] %in% lcr_survey$`Water System ID (pwsid)`, lcr_survey_filt$`Lead goosenecks/pigtails (leadgooseneck)`[lcr_survey_filt$`Water System ID (pwsid)` == data.clean$PWS.ID[x]], 2))
data.clean$sys_goose <- sapply(seq_along(data.clean$PWS.ID), function (x) ifelse(data.clean$PWS.ID[x] %in% lcr_survey$`Water System ID (pwsid)`, lcr_survey_filt$`Lead goosenecks/pigtails (sys_goose)`[lcr_survey_filt$`Water System ID (pwsid)` == data.clean$PWS.ID[x]], 2))
data.clean$leadpipe <- sapply(seq_along(data.clean$PWS.ID), function (x) ifelse(data.clean$PWS.ID[x] %in% lcr_survey$`Water System ID (pwsid)`, lcr_survey_filt$`Lead pipe, piping with lead-lined interior, or lead joint pipe in the distribution mains  (leadpipe)`[lcr_survey_filt$`Water System ID (pwsid)` == data.clean$PWS.ID[x]], 2))
data.clean$date.form.comp <- sapply(seq_along(data.clean$PWS.ID), function (x) ifelse(data.clean$PWS.ID[x] %in% lcr_survey$`Water System ID (pwsid)`, lcr_survey_filt$`Date Form Completed (completeddate)_2`[lcr_survey_filt$`Water System ID (pwsid)` == data.clean$PWS.ID[x]], 2))
data.clean$corrosion_control.nc.deq <- sapply(seq_along(data.clean$PWS.ID), function (x) ifelse(data.clean$PWS.ID[x] %in% lcr_survey$`Water System ID (pwsid)`, lcr_survey_filt$`Existing Corrosion Control Treatment (corrosioncontrol)`[lcr_survey_filt$`Water System ID (pwsid)` == data.clean$PWS.ID[x]], 2))
data.clean$galvanized <- sapply(seq_along(data.clean$PWS.ID), function (x) ifelse(data.clean$PWS.ID[x] %in% lcr_survey$`Water System ID (pwsid)`, lcr_survey_filt$`Galvanized piping, service lines, and home plumbing (galvanized)`[lcr_survey_filt$`Water System ID (pwsid)` == data.clean$PWS.ID[x]], 2))
data.clean$copper.mat <- sapply(seq_along(data.clean$PWS.ID), function (x) ifelse(data.clean$PWS.ID[x] %in% lcr_survey$`Water System ID (pwsid)`, lcr_survey_filt$`Copper from piping and alloys, service lines and home plumbing (copper)`[lcr_survey_filt$`Water System ID (pwsid)` == data.clean$PWS.ID[x]], 2))
data.clean$brass <- sapply(seq_along(data.clean$PWS.ID), function (x) ifelse(data.clean$PWS.ID[x] %in% lcr_survey$`Water System ID (pwsid)`, lcr_survey_filt$`Brass (brass)`[lcr_survey_filt$`Water System ID (pwsid)` == data.clean$PWS.ID[x]], 2))
data.clean$nc.deq.survey.86 <- sapply(seq_along(data.clean$PWS.ID), function (x) ifelse(data.clean$PWS.ID[x] %in% lcr_survey$`Water System ID (pwsid)`, lcr_survey_filt$`Construction under pre-1986 plumbing codes (pre1986)`[lcr_survey_filt$`Water System ID (pwsid)` == data.clean$PWS.ID[x]], 2))

data.clean <- data.clean %>%
  dplyr::mutate(
    corrosion.inhib_y.n.nc.deq = case_when(
      corrosion_control.nc.deq == 2 ~ "not in survey",
      grepl("corrosion|Corrsion|Yes|yes|phosphate|Phosphate|pH|PH|SODA|ASH|ORTHO|Soda|Ash|soda|ash|NAOH", corrosion_control.nc.deq) ~ "yes",
      !grepl("corrosion|Corrsion|Yes|yes|phosphate|Phosphate|pH|PH|SODA|ASH|ORTHO|Soda|Ash|soda|ash|NAOH", corrosion_control.nc.deq) ~ "no"
    ),
    lead.present = case_when(
      leadsolder == 2 ~ "not in survey",
      leadsolder|cus_goose|leadgooseneck|sys_goose|leadpipe|leadservice == 1 ~ "present",
      leadsolder|cus_goose|leadgooseneck|sys_goose|leadpipe|leadservice == 0 ~ "not present"
    ),
    corrosion.inhib_y.n.epa = case_when(
      PWS.ID %in% pws.id_reported.corrosion.inhib$`PWS ID` ~ "yes",
      !PWS.ID %in% pws.id_reported.corrosion.inhib$`PWS ID` ~ "no"
    )) %>%
  dplyr::mutate(corrosion.inhib = case_when(
                `corrosion.inhib_y.n.nc.deq` == "yes" | corrosion.inhib_y.n.epa == "yes" ~ "yes",
                `corrosion.inhib_y.n.nc.deq` == "no" | corrosion.inhib_y.n.epa == "no" ~ "no"))
  
# We make sure to get rid of the samples which do not report
# LCR as sampling point

lead <- data.clean %>%
  dplyr::filter(`Analyte Name` == "LEAD") %>%
  dplyr::distinct() %>%
  dplyr::arrange(desc(concentration.impute))

sampling.point <- lead %>%
  group_by(`Sampling Point`) %>%
  dplyr::summarize(n = n()) # 26,608 samples report LCR

lead <- lead %>%
  dplyr::filter(`Sampling Point` == "LCR")

# Violations: scrape from ncdeq as well as a series of csv downloads from epa ====

# Several separate tibbles are made
# Information presented in the paper are bound to the final "lead" tibble

violations <- read_csv("ncdeq.violations.scrape.csv") %>%
  dplyr::mutate(`Vio DeterminationDate` = mdy(`Vio DeterminationDate`)) %>%
  dplyr::select(-column_label)

violations.summary <- violations %>%
  dplyr::group_by(PWS.ID, `Violation Type Name`, `Analyte Name`, `Return To Compliance(Y=Yes; N=No)`) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::arrange(PWS.ID, desc(n)) %>%
  dplyr::ungroup()

violations.occt.summary <- violations.summary %>%
  dplyr::filter(grepl("OCCT", `Violation Type Name`)) %>%
  dplyr::mutate(
    corrosion.inhib_y.n.epa = case_when(
      PWS.ID %in% pws.id_reported.corrosion.inhib$`PWS ID` ~ "yes",
      !PWS.ID %in% pws.id_reported.corrosion.inhib$`PWS ID` ~ "no"
  )) %>%
  dplyr::arrange(corrosion.inhib_y.n.epa, `Return To Compliance(Y=Yes; N=No)`)

violations.pws.id.n <- violations %>%
  dplyr::group_by(PWS.ID) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::arrange(desc(n))

violations.unresolved <- violations %>%
  dplyr::filter(`Return To Compliance(Y=Yes; N=No)` == "N") %>%
  dplyr::arrange(`Vio DeterminationDate`)

violations.resolved <- violations %>%
  dplyr::filter(`Return To Compliance(Y=Yes; N=No)` == "Y") %>%
  dplyr::arrange(`Vio DeterminationDate`)

violations.occt.unresolved.not.in_epa.corrosion <- violations.unresolved %>%
  dplyr::filter(grepl("OCCT", `Violation Type Name`)) %>%
  dplyr::filter(!PWS.ID %in% pws.id_reported.corrosion.inhib$`PWS ID`)

violations.occt.unresolved.in_epa.corrosion <- violations.unresolved %>%
  dplyr::filter(grepl("OCCT", `Violation Type Name`)) %>%
  dplyr::filter(PWS.ID %in% pws.id_reported.corrosion.inhib$`PWS ID`)

violations.occt.resolved.not.in_epa.corrosion <- violations.resolved %>%
  dplyr::filter(grepl("OCCT", `Violation Type Name`)) %>%
  dplyr::filter(!PWS.ID %in% pws.id_reported.corrosion.inhib$`PWS ID`)

violations.occt.resolved.in_epa.corrosion <- violations.resolved %>%
  dplyr::filter(grepl("OCCT", `Violation Type Name`)) %>%
  dplyr::filter(PWS.ID %in% pws.id_reported.corrosion.inhib$`PWS ID`) %>%
  dplyr::arrange(PWS.ID)

violations.occt.resolved.last.date <- violations %>%
  dplyr::filter(grepl("OCCT", `Violation Type Name`)) %>%
  dplyr::filter(`Return To Compliance(Y=Yes; N=No)` == "Y") %>%
  dplyr::group_by(PWS.ID) %>%
  dplyr::mutate(last.violation = max(year(`Vio DeterminationDate`))) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(PWS.ID, .keep_all = TRUE)

nc.deq_open.occt.violation <- violations %>%
  dplyr::filter(grepl("OCCT", `Violation Type Name`)) %>%
  dplyr::filter(`Return To Compliance(Y=Yes; N=No)` == "N")
  
epa_violations.all <- read_csv(file = "epa_violations.csv") %>%
  dplyr::distinct() %>%
  dplyr::mutate(`RTC Date` = dmy(`RTC Date`),
                `Violation First Reported Date` = dmy(`Violation First Reported Date`)) 

# 427 fail to parse; NA's from violators that have not returned to compliance

epa_occt.violations <- epa_violations.all %>%
  dplyr::filter(grepl("OCCT|occt", `Violation Type`))

epa_violation.57 <- epa_violations.all %>%
  dplyr::filter(`Violation Code` == 57) %>%
  dplyr::distinct(`PWS ID`, `Violation First Reported Date`)

epa_violation.57.group <- epa_violations.all %>%
  dplyr::filter(`Violation Code` == 57) %>%
  dplyr::distinct(`PWS ID`, `Violation First Reported Date`) %>%
  dplyr::group_by(`PWS ID`) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::arrange(desc(n))

epa_open.occt.violation <- epa_occt.violations %>% 
  dplyr::filter(`Compliance Status` == "Open")
 
epa_occt.violations.RTC.last <- epa_occt.violations %>% 
  dplyr::filter(`Compliance Status` != "Open") %>%
  dplyr::arrange(`PWS ID`) %>%
  dplyr::group_by(`PWS ID`, `RTC Date`) %>%
  dplyr::mutate(number.of.RTC = n()) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(`PWS ID`, `RTC Date`, .keep_all = TRUE) %>%
  dplyr::group_by(`PWS ID`) %>%
  dplyr::mutate(last.comp.year.occt = max(year(`RTC Date`))) %>%
  dplyr::distinct(last.comp.year.occt, .keep_all = TRUE) %>%
  dplyr::ungroup() 

epa_violations.summary <- epa_violations.all %>%
  dplyr::distinct(`PWS ID`, `Violation Type`, `Compliance Status`, `Violation First Reported Date`) 

epa.install <- epa_violations.summary %>%
  dplyr::filter(grepl("install|Install", `Violation Type`))

epa_violations.pws.id.n <- epa_violations.all %>%
  dplyr::group_by(`PWS ID`) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(n))

lead$occt.resolve.year.nc.deq <- NA
lead$occt.resolve.year.nc.deq <- sapply(seq_along(lead$PWS.ID), function(x) if(lead$PWS.ID[x] %in% violations.occt.resolved.last.date$PWS.ID) {violations.occt.resolved.last.date$last.violation[violations.occt.resolved.last.date$PWS.ID == lead$PWS.ID[x]]} else{NA})

lead$occt.resolve.year.epa <- NA
lead$occt.resolve.year.epa <- sapply(seq_along(lead$PWS.ID), function(x) if(lead$PWS.ID[x] %in% epa_occt.violations.RTC.last$`PWS ID`) {epa_occt.violations.RTC.last$last.comp.year.occt[epa_occt.violations.RTC.last$`PWS ID` == lead$PWS.ID[x]]} else{NA})

lead <- lead %>%
  dplyr::mutate(occt.violation_y.n = case_when(
    PWS.ID %in% c(epa_violation.57.group$`PWS ID`, violations.occt.summary$PWS.ID) ~ "yes",
    !PWS.ID %in% c(epa_violation.57.group$`PWS ID`, violations.occt.summary$PWS.ID) ~ "no"
  ),
  lcr.violation = case_when(
    PWS.ID %in% c(epa_violations.all$`PWS ID`, violations$PWS.ID) ~ "yes",
    !PWS.ID %in% c(epa_violations.all$`PWS ID`, violations$PWS.ID) ~ "no"
  ),
  occt_open.violation_y.n = case_when(
    PWS.ID %in% c(epa_open.occt.violation$`PWS ID`, nc.deq_open.occt.violation$PWS.ID) ~ "yes",
    !PWS.ID %in% c(epa_open.occt.violation$`PWS ID`, nc.deq_open.occt.violation$PWS.ID) ~ "no",
  )) %>%
  dplyr::mutate(color.range = 
                  case_when(
                    start.year < 1987 ~ "< 1987",
                    start.year >= 1987 & start.year < 1991 ~ "1987-1990",
                    start.year >= 1991 & start.year < 2014 ~ "1991-2013",
                    start.year >= 2014 ~ "> 2013"
                  )) 

# This is the final lead tibble

# Tables and comments presented in the manuscript ====

# Table 1 ====

# trigger

lead.trigger.MDL <- lead %>%
  dplyr::filter(`Less than Indicator` == "Y") %>%
  dplyr::filter(`Reporting Level` >= 0.005) %>%
  dplyr::group_by(start.year) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::ungroup()

lead.above.trigger <- lead %>%
  dplyr::mutate(above.trigger = case_when(
    concentration.impute >= 0.005 ~ "yes",
    !concentration.impute >= 0.005 ~ "no"
  )) %>%
  dplyr::group_by(start.year) %>%
  dplyr::mutate(no.samples = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(start.year, above.trigger) %>%
  dplyr::summarize(n = n()) %>%
  tidyr::spread(above.trigger, n) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(yes = as.numeric(yes),
                no = as.numeric(no)) %>%
  dplyr::mutate(yes = case_when(
    is.na(yes) ~ 0,
    !is.na(yes) ~ yes
  ),
  no = case_when(
    is.na(no) ~ 0,
    !is.na(no) ~ no
  )) %>%
  dplyr::mutate(n = no + yes)

lead.above.trigger$trigger.MDL <- NA

lead.above.trigger$trigger.MDL <- sapply(seq_along(lead.above.trigger$start.year), function (x) lead.trigger.MDL$n[lead.trigger.MDL$start.year == lead.above.trigger$start.year[x]])

lead.above.trigger <- lead.above.trigger %>%
  dplyr::mutate(trigger.MDL = as.numeric(trigger.MDL)) %>%
  dplyr::mutate(trigger.MDL = case_when(
    is.na(trigger.MDL) ~ 0,
    !is.na(trigger.MDL) ~ trigger.MDL
  )) %>%
  dplyr::mutate(yes.minus.MDL = yes - trigger.MDL,
                n.minus.MDL = n - trigger.MDL) %>%
  dplyr::mutate(percent.trigger = round((yes.minus.MDL/n.minus.MDL) * 100, digits = 0)) %>%
  dplyr::arrange(desc(percent.trigger)) %>%
  dplyr::filter(percent.trigger != 100) %>%
  dplyr::arrange(start.year)

write.csv(lead.above.trigger ,"trigger.csv", row.names = FALSE)

# action

lead.action.MDL <- lead %>%
  dplyr::filter(`Less than Indicator` == "Y") %>%
  dplyr::filter(`Reporting Level` >= 0.015) %>%
  dplyr::group_by(start.year) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::ungroup()

lead.above.action <- lead %>%
  dplyr::mutate(above.action = case_when(
    concentration.impute >= 0.015 ~ "yes",
    !concentration.impute >= 0.015 ~ "no"
  )) %>%
  dplyr::group_by(start.year) %>%
  dplyr::mutate(no.samples = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(start.year, above.action) %>%
  dplyr::summarize(n = n()) %>%
  tidyr::spread(above.action, n) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(yes = as.numeric(yes),
                no = as.numeric(no)) %>%
  dplyr::mutate(yes = case_when(
    is.na(yes) ~ 0,
    !is.na(yes) ~ yes
  ),
  no = case_when(
    is.na(no) ~ 0,
    !is.na(no) ~ no
  )) %>%
  dplyr::mutate(n = no + yes) 

lead.above.action$action.MDL <- NA

lead.above.action$action.MDL <- sapply(seq_along(lead.above.action$start.year), function (x) lead.action.MDL$n[lead.action.MDL$start.year == lead.above.action$start.year[x]])

lead.above.action <- lead.above.action %>%
  dplyr::mutate(action.MDL = as.numeric(action.MDL)) %>%
  dplyr::mutate(action.MDL = case_when(
    is.na(action.MDL) ~ 0,
    !is.na(action.MDL) ~ action.MDL
  )) %>%
  dplyr::mutate(yes.minus.MDL = yes - action.MDL,
                n.minus.MDL = n - action.MDL) %>%
  dplyr::mutate(percent.action = round((yes.minus.MDL/n.minus.MDL) * 100, digits = 0)) %>%
  dplyr::arrange(desc(percent.action)) %>%
  dplyr::filter(percent.action != 100) %>%
  dplyr::arrange(start.year)

write.csv(lead.above.action, "action.csv", row.names = FALSE)

# Final edits were made in Excel

# Table 1 ====

# "110 Code 57 violations from 82 of the 206 PWS...51 of the PWS report corrosion control"

epa_violation.57
epa_violation.57.group

lead %>%
  dplyr::filter(corrosion.inhib_y.n.epa == "yes") %>%
  dplyr::filter(PWS.ID %in% epa_violation.57$`PWS ID`) %>%
  dplyr::distinct(PWS.ID) %>%
  nrow()

# "110 Code 57 violations from 82 of the 206 PWS...51 report corrosion control"

# "Out of all 206 facilities evaluated, 79 report using a form of corrosion control to the EPA,
# including 53 out of 116 which were built before 1987"

lead %>%
  dplyr::filter(corrosion.inhib_y.n.epa == "yes") %>%
  dplyr::distinct(PWS.ID) %>%
  nrow()

lead %>%
  dplyr::filter(start.year < 1987) %>%
  dplyr::filter(corrosion.inhib_y.n.epa == "yes") %>%
  dplyr::distinct(PWS.ID) %>%
  nrow()

lead %>%
  dplyr::filter(start.year < 1987) %>%
  dplyr::distinct(PWS.ID) %>%
  nrow()

# "Out of all 206 facilities evaluated, 87 report using a form of corrosion control,
# including 57 out of 116 which were built before 1987"

# PWS built prior to 1987 that resolved a Code 57 violation "

lead %>%
  dplyr::filter(start.year < 1987) %>%
  dplyr::filter(occt.violation_y.n == "yes") %>%
  dplyr::group_by(PWS.ID) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(PWS.ID, corrosion.inhib_y.n.epa, occt.resolve.year.epa) %>%
  dplyr::group_by(corrosion.inhib_y.n.epa) %>%
  dplyr::summarize(n = n(),
                   med = median(occt.resolve.year.epa),
                   max = max(occt.resolve.year.epa),
                   min = min(occt.resolve.year.epa))

# PWS built prior to 1987 that resolved a Code 57 violation "

# 149 of the 206 filled out the LCR survey

lead %>%
  dplyr::filter(nc.deq.survey.86 %in% c(0,1)) %>%
  dplyr::distinct(PWS.ID) %>%
  nrow()

# 149 of the 206 filled out the survey

# 50 of those reported lead in the system

lead %>%
  dplyr::filter(lead.present == "present") %>%
  dplyr::distinct(PWS.ID) %>%
  nrow()

# 50 of those reported lead in the system

# with 33 of those reporting corrosion control

lead %>%
  dplyr::filter(lead.present == "present" & corrosion.inhib_y.n.epa == "yes") %>%
  dplyr::distinct(PWS.ID) %>%
  nrow()

# with 33 of those reporting corrosion control

# Table 2 ====

lead.MDL <- lead %>%
  dplyr::filter(`Less than Indicator` == "Y") %>%
  dplyr::group_by(`Reporting Level`) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::mutate(percent.of.total = (n/nrow(lead)*100)) %>%
  dplyr::arrange(desc(percent.of.total))

# Final formatting was done in Excel

# Table 2 ====

# Table S2 ====

corrosion.technique.summary <- pws.id_reported.corrosion.inhib %>%       
  dplyr::select(`PWS ID`, `Treatment Process`) %>%                       
  distinct() %>%                                                         
  dplyr::group_by(`Treatment Process`) %>%                               
  dplyr::summarize(n = n()) %>%                                          
  dplyr::arrange(desc(n)) # Some PWS use more than one type of treatment 

# Table S2 ====

method.summary <- lead %>%
  dplyr::filter(`Method Code` != "null") %>%
  dplyr::group_by(`Method Code`) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::arrange(desc(n))

# Table S3 ====

method.summary.level <- lead %>%
  dplyr::filter(`Method Code` != "null") %>%
  dplyr::filter(`Less than Indicator` == "Y") %>%
  dplyr::mutate(Instrument = case_when(
    `Method Code` == "200.8" ~ "ICP-MS",
    `Method Code` %in% c("3113B", "200.9") ~ "GFAAS",
    `Method Code` == "200.5" ~ "ICP OES",
    `Method Code` == "3111B" ~ "FAAS"
  )) %>%
  dplyr::group_by(`Method Code`, `Level Type`, Instrument, concentration.impute) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::arrange(`Method Code`)

# Table S3 ====

# ratio detects over non detects

# 19 % detectable lead in 26,608 samples

nrow(lead %>% dplyr::filter(`Less than Indicator` == "N"))/nrow(lead)

# 19 % detectable lead in 26,608 samples

# normality

# central limit theorem, but we have censored data. Can't compare means

lead %>%
  dplyr::filter(`Less than Indicator` == "N") %>%
  ggplot(aes(sample = concentration.impute)) +
  geom_qq()

# 90th percentile is 6 ppb 

quantile(lead %>%
           dplyr::arrange(desc(concentration.impute)) %>%
           dplyr::pull(concentration.impute),
         probs = seq(0, 1, by = 0.1)) 

# 90th percentile is 6 ppb

# Figures ====

# define consistent ggplot theme to apply to all figures
theme_master <- function(base_size=14, base_family="Times") {
  library(grid)
  (theme_bw(base_size = base_size, base_family = base_family)+
      theme(text=element_text(color="black"),
            axis.title=element_text(face="bold", size = rel(1.3)),
            axis.text=element_text(size = rel(0.8), color = "black"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
            legend.title=element_text(face="bold"),
            legend.text=element_text(face="bold"),
            legend.background=element_rect(fill="transparent"),
            legend.key.size = unit(1, 'lines'),
            strip.text = element_text(size = rel(0.8), color = "black"),
            panel.border=element_rect(color="black",size=1),
            panel.grid=element_blank()
      ))
}

# Fig. 1 ====

lead %>%
  dplyr::filter(`Less than Indicator` == "N") %>% # only three build date ranges after filtering for detectable lead
  ggplot(aes(x = factor(color.range, levels = c("< 1987", "1987-1990", "1991-2013")), y = concentration.impute * 1000)) +
  geom_hline(aes(yintercept = 15,
                 linetype = "dashed")) +
  geom_hline(aes(yintercept = 5,
                 linetype = "solid")) +
  scale_linetype_manual(name = "",
                        values = c("solid", "dashed"),
                        labels = c("Action level", "Trigger level")) +
  geom_quasirandom(aes(fill = factor(color.range, levels = c("< 1987", "1987-1990", "1991-2013"))
                       ),
                   show.legend = FALSE,
                   color = "black",
                   alpha = 0.8,
                   shape = 21,
                   groupOnX = TRUE) +
  scale_fill_manual(values = viridis(3, 0.8, direction = 1)) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(x = "Build Date Range",
       y = expression(bold(paste("Pb", " (", mu, "g/L)", sep = "")))) +
       #y = expression(bold(paste("Log ", "[", Pb["("*mu*g~"/L)"], "]", sep = "")))) +
  guides(fill = NULL) +
  theme_master(base_size = 16)

lead %>%
  dplyr::filter(`Less than Indicator` == "N") %>% # only three build date ranges after filtering for detectable lead
  dplyr::group_by(color.range) %>%
  dplyr::summarize(n = n())

ggsave("Fig_1.tiff", units = "in", dpi = 1000, compression = "lzw", width = 6, height = 4)
dev.off()

# Fig. 1 ====

# Fig. 2 ====

lead %>% 
  dplyr::filter(`Less than Indicator` == "N") %>%
  ggplot(aes(x = collection.year, y = concentration.impute*1000)) +
  #geom_violin() +
  #coord_cartesian(ylim = c(0, 0.1)) +
  geom_hline(aes(yintercept = 15,
                 linetype = "dashed")) +
  geom_hline(aes(yintercept = 5,
                 linetype = "solid")) +
  scale_linetype_manual(name = "",
                        values = c("solid", "dashed"),
                        labels = c("Action level", "Trigger level")) +
  geom_quasirandom(aes(fill = start.year),
    alpha = 0.8,
    shape = 21,
    groupOnX = TRUE) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_fill_viridis_c(direction = 1,
                       alpha = 0.8,
                       breaks = seq(min(lead$start.year), max(lead$start.year), by = 10)) +
  scale_x_continuous(breaks = seq(min(lead$collection.year), max(lead$collection.year), 5)) +
  guides(fill = guide_colorbar(order = 1),
         linetype = guide_legend(order = 2)) +
  labs(x = "Collection Year",
       fill = "Build Date",
       y = expression(bold(paste("Pb", " (", mu, "g/L)", sep = ""))),
       linetype = NULL) +
  theme_master(base_size = 14) 

lead %>% 
  dplyr::filter(`Less than Indicator` == "N") %>%
  dplyr::summarize(n = n())

ggsave("Fig_2.tiff", compression = "lzw", units = "in", width = 6, height = 4, dpi = 1000)
dev.off()  

# Fig. 2 =====

# Fig. 3 ====

# Fig. 3 was produced in Excel using the following data

fig.3 <- lead %>%
  dplyr::filter(occt.violation_y.n == "yes") %>%
  dplyr::filter(start.year < 1987) %>%
  dplyr::mutate(corrosion.inhib_y.n.epa = case_when(
    corrosion.inhib_y.n.epa == "no" ~ "No Corrosion Control",
    corrosion.inhib_y.n.epa == "yes" ~ "Corrosion Control"
  )) %>%
  dplyr::select(collection.year, concentration.impute, corrosion.inhib_y.n.epa, `Less than Indicator`) %>%
  dplyr::arrange(collection.year, desc(concentration.impute), corrosion.inhib_y.n.epa, `Less than Indicator`) %>%
  dplyr::group_by(collection.year, corrosion.inhib_y.n.epa, `Less than Indicator`) %>%
  dplyr::mutate(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(collection.year, corrosion.inhib_y.n.epa) %>%
  dplyr::mutate(num.samples = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(percent.detect = (n/num.samples) * 100) %>%
  dplyr::filter(collection.year >= 2010 & collection.year <= 2018) %>%
  dplyr::filter(`Less than Indicator` == "N")

fig.3 %>%
  dplyr::filter(corrosion.inhib_y.n.epa == "Corrosion Control") %>%
  write.csv("corrosion.csv", row.names = FALSE)

fig.3 %>%
  dplyr::filter(corrosion.inhib_y.n.epa == "No Corrosion Control") %>%
  write.csv("no.corrosion.csv", row.names = FALSE)

# Fig. 4 ====

# Fig. 4 ====

# for loop to produce figs 

no.cor.single <- "NC0276611"
cor.single <- "NC0150461"
cor.worst.case <- "NC0187548"
no.cor.best.case <- "NC0235515"

pws.cor.plots <- c(no.cor.single, no.cor.best.case,
                   cor.single, cor.worst.case)

for (i in seq_along(pws.cor.plots)) {

lead %>% 
  dplyr::filter(PWS.ID == pws.cor.plots[i]) %>%
  dplyr::filter(`Less than Indicator` == "N") %>%
  ggplot(aes(x = collection.year, y = concentration.impute*1000)) +
  geom_hline(data = lead %>%
             dplyr::filter(PWS.ID %in% pws.cor.plots[i]),
             aes(yintercept = 15,
                 linetype = "Action level"),
             show.legend = TRUE) +
  geom_hline(data = lead %>%
               dplyr::filter(PWS.ID == pws.cor.plots[i]) ,
             aes(yintercept = 5,
                 linetype = "Trigger level"),
             show.legend = TRUE) +
  geom_vline(data = lead %>%
               dplyr::filter(PWS.ID == pws.cor.plots[i]),
             aes(xintercept = occt.resolve.year.epa,
                 linetype = "OCCT resolved"),
             show.legend = TRUE) +
  scale_linetype_manual(name = NULL, labels = c("Action level", "Trigger level", "OCCT resolved"), values = c("Action level" = 1, "Trigger level" = 2, "OCCT resolved" = 3)) +
  geom_quasirandom(size = 2.5,
                   shape = 21,
                   fill = "transparent",
                   color = "black",
                   groupOnX = TRUE) +
  scale_y_log10(
    limits = c(1, 50000),
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_continuous(breaks = seq(1991, 2019, 5),
                     limits = c(1991, 2019)) +
  labs(x = "Collection Year",
       y = expression(bold(paste("Pb", " (", mu, "g/L)", sep = ""))),
       #y = expression(bold(paste("Log ", "[", Pb["("*mu*g~"/L)"], "]", sep = ""))),
       linetype = "") +#,
  #fill = "Sample collection") +
  theme_master(base_size = 16) 
  
ggsave(paste(pws.cor.plots[i], ".tiff", sep = ""), units = "in", width = 6, height = 4, compression = "lzw", dpi = 1000)
}

# Fig. 4 ====

# Fig. 5 ====

# Fig. 5 ====

lead %>%
  dplyr::filter(`Less than Indicator` == "N" & lead.present == "present") %>%
  dplyr::mutate(corrosion.inhib_y.n.epa = case_when(
    corrosion.inhib_y.n.epa == "no" ~ "No Corrosion Control",
    corrosion.inhib_y.n.epa == "yes" ~ "Corrosion Control"
  )) %>%
  ggplot(aes(x = collection.year, y = concentration.impute*1000)) +
  geom_hline(aes(yintercept = 15,
                 linetype = "Action level")) +
  geom_hline(aes(yintercept = 5,
                 linetype = "Trigger level")) +
  geom_quasirandom(aes(fill = start.year),
                   shape = 21,
                   alpha = 0.8,
                   groupOnX = TRUE) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_fill_viridis_c(direction = 1,
                       breaks = seq(min(lead %>% dplyr::filter(`Less than Indicator` == "N" & lead.present == "present") %>% dplyr::select(start.year)), max(lead %>% dplyr::filter(`Less than Indicator` == "N" & lead.present == "present") %>% dplyr::select(start.year)), by = 10)) +
  scale_x_continuous(breaks = seq(min(lead %>% dplyr::filter(`Less than Indicator` == "N" & lead.present == "present") %>% dplyr::select(collection.year)), max(lead %>% dplyr::filter(`Less than Indicator` == "N" & lead.present == "present") %>% dplyr::select(collection.year)), 5)) +
  guides(fill = guide_colorbar(order = 1),
         linetype = guide_legend(order = 2)) +
  labs(x = "Collection Year",
       fill = "Build Date",
       y = expression(bold(paste("Pb", " (", mu, "g/L)", sep = ""))),
       linetype = NULL) +
  theme_master(base_size = 14) +
  facet_wrap(~corrosion.inhib_y.n.epa)

lead %>%
  dplyr::filter(`Less than Indicator` == "N" & lead.present == "present")%>%
  dplyr::group_by(corrosion.inhib_y.n.epa) %>%
  dplyr::summarize(n = n())

ggsave("Fig_5.tiff", compression = "lzw", units = "in", width = 6, height = 4, dpi = 1000)
dev.off() 

# Fig. 5 ====