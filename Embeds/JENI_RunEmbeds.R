#### Set up data and environment ####
library(here)
library(DT)
# devtools::install_github("rstudio/DT")
library(RPostgreSQL)
library(formattable)
library(knitr)
library(dplyr)
library(sf)
library(leaflet)
# devtools::install_github('bhaskarvk/leaflet.extras')
library("leaflet.extras")
library(htmltools)
library(htmlwidgets)
library(rpostgis)
# install.packages("rmapshaper")
library(rmapshaper)

# connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("prop47")

# run data


#### JENI Prep Steps ####

### Load index data ###
jeni<- st_read(con, query = "SELECT * FROM jeni_jesi_2021.jeni") 

# round index data
jeni <- mutate(jeni,
               jeni_pctl = round(jeni_pctl,3)*100,
               jeni_score = round(jeni_score,3),
               system_pctl = round(system_pctl,3)*100,
               drivers_pctl = round(drivers_pctl,3)*100,
               risk_pctl = round(risk_pctl,3)*100,
               population_count = round(population_count,0),
               black_rate = round(black_rate,1),
               latinx_rate = round(latinx_rate,1),
               aian_rate = round(aian_rate,1),
               nhpi_rate = round(nhpi_rate,1),
               nohighschool_rate = round(nohighschool_rate,1),
               below200fpl_rate = round(below200fpl_rate,1),
               unemployed_rate = round(unemployed_rate,1),
               mentalhlth_hosp_rate = round(mentalhlth_hosp_rate,1),
               alcdrug_hosp_rate = round(alcdrug_hosp_rate,1),
               vc_rate = round(vc_rate,1),
               homeless_rate = round(homeless_rate,1)   
)

# add sd and spa names
jeni<-jeni%>%mutate(sd_name=recode(sd_number,
                                   "1"="Solis",
                                   "2"="Mitchell",
                                   "3"="Kuehl",
                                   "4"="Hahn",
                                   "5"="Barger"))%>%
  mutate(spa_name=recode(spa_number,
                         "1"="Antelope Valley",
                         "2"="San Fernando Valley",
                         "3"="San Gabriel Valley",
                         "4"="Metro",
                         "5"="West",
                         "6"="South",
                         "7"="East",
                         "8"="South Bay"
  ))

# transform for leaflet projection
jeni<-st_transform(jeni, 4326)


## Create dataset for zips with no index score--no or low population###
# polygons
zips <- st_read(con, query = "SELECT * from jeni_jesi_2021.jeni_jesi_zip_polys")

# transform for leaflet projection
zips <- st_transform(zips, 4326)

# bring in population for pop-up
pop <- st_read(con, query = "SELECT * from jeni_jesi_2021.jeni_jesi_zip_population")

# Add population for missing JENI scores to popup
pop <- rename(pop, zipcode = lac_zip)

# join geographies to population and clean up
nojeni <- left_join(zips, pop ,by="zipcode")
nojeni <- filter(nojeni, population_count<500 | is.na(population_count))
nojeni$population_count <- prettyNum(round(nojeni$population_count,0), big.mark = ",", preserve.width = "none")

# nojeni: keep geom (it's the one transformed to 4326), drop geom_2229
nojeni <- nojeni %>% select(-geom_2229)

### Overlay Prep ###
# load spas and sds
sds <- st_read(con, query = "SELECT * from jeni_jesi_2021.lacounty_supdis_2021")
sds <- sds %>% select(-geom_3310) # has 2 geoms and only needs 1 (geom_2229 was transformed in place to 4326, so drop 3310)

spas <- st_read(con, query = "SELECT * FROM jeni_jesi_2021.la_county_service_planning_areas_2012")

# re-project spas and sds to 4326 for leaflet
sds<-st_transform(sds, 4326)
spas<-st_transform(spas, 4326)


## Load jesi 2021 ##
jesi_2021 <- st_read(con, query = "SELECT * from jeni_jesi_2021.jesi")
jesi_2021 <-st_transform(jesi_2021, 4326)
# filter for low and lowest only to include
jesi_2021_lowest <- filter(jesi_2021, jesi_category=="Lowest" | jesi_category=="Low")


### Simplify Shapes ###
jeni <- ms_simplify(jeni)
nojeni <- ms_simplify(nojeni)
sds <- ms_simplify(sds)
spas <- ms_simplify(spas)
# jeni_2019_highest<-ms_simplify(jeni_2019_highest)
jesi_2021_lowest<-ms_simplify(jesi_2021_lowest)

# recover integers in the jeni rank column - ms_simplify may have messed up if stored as bigint in postgres table
recover_i64 <- function(x) {
  class(x) <- "integer64"
  as.integer(x)
}

jeni$jeni_rank <- recover_i64(jeni$jeni_rank)
head(jeni$jeni_rank)

###RUN EMBEDS###

rmarkdown::render(input = here("Embeds/Figure1_JENI_Map.Rmd"),
                  output_format = "html_document",
                  output_file = paste0("Figure1_JENI_Map.html"),   
                  output_dir = here("Embeds"))


rmarkdown::render(input = here("Embeds/Figure2_JENI_Components_Map.Rmd"),
                  output_format = "html_document",
                  output_file = paste0("Figure2_JENI_Components_Map.html"),   
                  output_dir = here("Embeds"))

rmarkdown::render(input = here("Embeds/Figure3_JENI_Table.Rmd"),
                  output_format = "html_document",
                  output_file = paste0("Figure3_JENI_Table.html"),   
                  output_dir = here("Embeds"))

