library(data.table)
library(ggplot2)

mig <- fread("C:/Users/ngraetz/Downloads/Data on the global flow of people_Version March2014.csv")
mig <- mig[country_orig!=country_dest, ]
mig_countries <- unique(mig[, country_orig])

#########################################################################################################
###################### Load population data
#########################################################################################################
create_changes <- function(n, dt, vars, lag, change=FALSE, year_step=5) {
  dt.n <- dt[name==n, ]
  if(length(dt.n[, name])==1) return(NULL)
  if(length(dt.n[, name])!=1) {
    for(r in seq(min(dt.n[, year]),max(dt.n[, year]),year_step)) {
      for(v in vars) {
        if(r>=min(dt.n[, year])+lag & change==TRUE) {
          previous_v <- dt.n[year==r-lag, get(v)]
          dt.n[year==r, paste0('relchange_',(v)) := (get(v)-previous_v)/previous_v]
          dt.n[year==r, paste0('abschange_',(v)) := get(v)-previous_v]
          dt.n[year==r, paste0('r_',(v)) := log(get(v)/previous_v)/lag]
        }
        if(r>=min(dt.n[, year])+lag) {
          lag_v <- dt.n[year==r-lag, get(v)]
          dt.n[year==r, paste0('lag',lag,'_',(v)) := lag_v]
        }
      }
    }
    return(dt.n)
  }
}
pops <- fread('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/WPP2017_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.csv', skip=1)
names(pops) <- c('Index','Variant','name','Notes','country_code','year',paste0('age',as.character(c(0:79))), 'age80plus', paste0('age',as.character(c(80:100))))
cols <- grep("^age", names(pops), value = TRUE)
for(c in cols) {
  pops[, (c) := as.numeric(gsub(' ','',get(c)))]
}
pops <- pops[, total_pop := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=grep("^age", names(pops))]
total_pops <- pops[, c('name','year','total_pop')]
total_pops[, total_pop := total_pop * 1000]
for(c in cols) {
  pops[, paste0('prop_',c) := get(c) / total_pop]
}
pops <- pops[, size_15_19 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('age15','age16','age17','age18','age19')]
pops <- pops[, prop_15_19 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('prop_age15','prop_age16','prop_age17','prop_age18','prop_age19')]
pops <- pops[, size_10_19 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('age10','age11','age12','age13','age14','age15','age16','age17','age18','age19')]
pops <- pops[, prop_10_19 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('prop_age10','prop_age11','prop_age12','prop_age13','prop_age14','prop_age15','prop_age16','prop_age17','prop_age18','prop_age19')]
pops <- pops[, size_20_24 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('age20','age21','age22','age23','age24')]
pops <- pops[, prop_20_24 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('prop_age20','prop_age21','prop_age22','prop_age23','prop_age24')]
pops <- pops[, prop_25_29 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('prop_age25','prop_age26','prop_age27','prop_age28','prop_age29')]
pops <- pops[, size_25_29 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('age25','age26','age27','age28','age29')]
pops <- pops[, size_15_24 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('age15','age16','age17','age18','age19','age20','age21','age22','age23','age24')]
pops <- pops[, size_15_29 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('age15','age16','age17','age18','age19','age20','age21','age22','age23','age24','age25','age26','age27','age28','age29')]
for(y in seq(1950,2015,5)) pops[year>=y, five_year := y]
pops <- pops[, list(prop_20_24=mean(prop_20_24), prop_25_29=mean(prop_25_29), size_25_29=mean(size_25_29),
                    prop_15_19=mean(prop_15_19), size_15_19=mean(size_15_19), size_20_24=mean(size_20_24),
                    prop_10_19=mean(prop_10_19), size_10_19=mean(size_10_19), size_15_24=mean(size_15_24), size_15_29=mean(size_15_29)), by=c('country_code','name','five_year')]
setnames(pops, 'five_year', 'year')
pops[, ratio_15_19_20_24 := size_15_19 / size_20_24]
pops[, prop_15_29 := prop_15_19 + prop_20_24 + prop_25_29]
pops <- pops[, c('name','year','ratio_15_19_20_24','prop_15_29','size_15_19','size_20_24','size_25_29','size_10_19','size_15_24','size_15_29')]
pops <- rbindlist(lapply(unique(pops[, name]), create_changes,
                         dt = pops,
                         vars = c('size_15_19','size_20_24','size_25_29'),
                         change = TRUE,
                         lag = 5))  
pops <- rbindlist(lapply(unique(pops[, name]), create_changes,
                         dt = pops,
                         vars = c('size_10_19','size_15_24'),
                         change = TRUE,
                         lag = 10))
pops <- rbindlist(lapply(unique(pops[, name]), create_changes,
                         dt = pops,
                         vars = c('size_15_29'),
                         change = TRUE,
                         lag = 15))
## Calculate all possible lags of r for migration correlation.
for(l in c(0,5)) {
  pops <- rbindlist(lapply(unique(pops[, name]), create_changes,
                           dt = pops,
                           vars = c('r_size_20_24','r_size_25_29','r_size_10_19','r_size_15_24','r_size_15_19','r_size_15_29'),
                           change = FALSE,
                           lag = l))  
}
## Fix countries
mig[!(country_orig %in% pops[, name]), unique(country_orig)]
pops[grep('Ivoire',name), name := 'Ivory Coast']
pops[grep('United States of America',name), name := 'United States']
pops[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
pops[name=='Cabo Verde', name := 'Cape Verde']
pops[grep('United Republic of Tanzania',name), name := 'Tanzania']
pops[grep('Bosnia and Herzegovina',name), name := 'Bosnia & Herzegovina']
pops[grep('Czechia',name), name := 'Czech Republic']
pops[grep('TFYR Macedonia',name), name := 'Macedonia']
pops[grep('Republic of Moldova',name), name := 'Moldova']
pops[grep('Russian Federation',name), name := 'Russia']
pops[grep('Syrian Arab Republic',name), name := 'Syria']
pops[grep('Iran',name), name := 'Iran']
pops[name=='Republic of Korea', name := 'South Korea']
pops[name=="Dem. People's Republic of Korea", name := 'North Korea']
pops[grep('Brunei Darussalam',name), name := 'Brunei']
pops[name=="Lao People's Democratic Republic", name := 'Laos']
pops[grep('Viet Nam',name), name := 'Vietnam']
pops[grep('Bolivia',name), name := 'Bolivia']
pops[grep('Trinidad and Tobago',name), name := 'Trinidad & Tobago']
pops[grep('Saint Vincent and the Grenadines',name), name := 'Saint Vincent & the Grenadines']
pops[grep('Venezuela',name), name := 'Venezuela']
mig[!(country_orig %in% pops[, name]), unique(country_orig)]
setnames(pops, 'name', 'country')
saveRDS(pops, 'C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/processed_pops.RDS')

pops <- fread("C:/Users/ngraetz/Downloads/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.csv")
names(pops) <- c('Index','Variant','name','Notes','country_code','type','parent','year',paste0('age',as.character(seq(0,100,5))))
pops <- melt(pops, id.vars=c('name','year'), measure.vars = grep('^age',names(pops)), value.name = 'pop', variable.name = 'age')
pops[, pop := gsub(' ','',pop)]
pops[, pop := as.numeric(pop)*1000]
pops[, age := as.numeric(gsub('age','',age))]
mig[!(country_orig %in% pops[, name]), unique(country_orig)]
pops[name=='North Macedonia', name := 'Macedonia']
pops[grep('Eswa',name), name := 'Swaziland']
pops[grep('Ivoire',name), name := 'Ivory Coast']
pops[grep('United States of America',name), name := 'United States']
pops[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
pops[name=='Cabo Verde', name := 'Cape Verde']
pops[grep('United Republic of Tanzania',name), name := 'Tanzania']
pops[grep('Bosnia and Herzegovina',name), name := 'Bosnia & Herzegovina']
pops[grep('Czechia',name), name := 'Czech Republic']
pops[grep('TFYR Macedonia',name), name := 'Macedonia']
pops[grep('Republic of Moldova',name), name := 'Moldova']
pops[grep('Russian Federation',name), name := 'Russia']
pops[grep('Syrian Arab Republic',name), name := 'Syria']
pops[grep('Iran',name), name := 'Iran']
pops[name=='Republic of Korea', name := 'South Korea']
pops[name=="Dem. People's Republic of Korea", name := 'North Korea']
pops[grep('Brunei Darussalam',name), name := 'Brunei']
pops[name=="Lao People's Democratic Republic", name := 'Laos']
pops[grep('Viet Nam',name), name := 'Vietnam']
pops[grep('Bolivia',name), name := 'Bolivia']
pops[grep('Trinidad and Tobago',name), name := 'Trinidad & Tobago']
pops[name=="Saint Vincent & the Grenadines", name := 'Saint Vincent & the Grenadines']
pops[grep('Venezuela',name), name := 'Venezuela']
mig[!(country_orig %in% pops[, name]), unique(country_orig)]
setnames(pops, 'name', 'country')

## Make total pops
total_pops <- pops[, list(total_pop=sum(pop)), by=c('year','country')]
total_pops <- pops[, c('name','year','total_pop')]
saveRDS(total_pops, 'C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/total_pops.RDS')

## Make all growth rates
growth <- copy(pops)
growth[, l.pop := shift(pop), by=c('age','country')]
growth[, r := (log(pop/l.pop)/5)*100]
growth[, l.r := shift(r), by=c('age','country')]
growth <- dcast(growth, country + year ~ age, value.var = c('r','l.r'))
saveRDS(growth, 'C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/growth.RDS')

growth <- copy(pops)
growth <- growth[age %in% c(15,20), list(pop=sum(pop)), by=c('country','year')]
growth[, l.pop := shift(pop), by=c('country')]
growth[, r.15.24 := (log(pop/l.pop)/5)*100]
growth[, l.r.15.24 := shift(r.15.24), by=c('country')]
saveRDS(growth, 'C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/growth_15_24.RDS')

#########################################################################################################
###################### Load population data
#########################################################################################################
## World Bank Income Groups and GBD Regions
wb_ids <- fread('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/paper_figs/wb_gbd_ids.csv')
setnames(wb_ids, 'location_name', 'name')
mig[!(country_orig %in% wb_ids[, name]), unique(country_orig)]
wb_ids[grep('Ivoire',name), name := 'Ivory Coast']
wb_ids[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
wb_ids[grep('The Gambia',name), name := 'Gambia']
wb_ids[grep('Bosnia',name), name := 'Bosnia & Herzegovina']
wb_ids[grep('Russian Federation',name), name := 'Russia']
wb_ids[grep('Federated States of Micronesia',name), name := 'Micronesia']
mig[!(country_orig %in% wb_ids[, name]), unique(country_orig)]
wb_ids <- wb_ids[, c('name','region_name')]
setnames(wb_ids, 'name', 'country')
saveRDS(wb_ids, 'C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/wb_ids.RDS')

## Make LDI per capita growth rates
ldi <- fread('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/ldi.csv')
setnames(ldi, 'mean_value', 'ldi_pc')
setnames(ldi, 'year_id', 'year')
setnames(ldi, 'location_id', 'loc_id')
setnames(ldi, 'location_name', 'name')
mig[!(country_orig %in% ldi[, name]), unique(country_orig)]
ldi[grep('Ivoire',name), name := 'Ivory Coast']
ldi[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
ldi[grep('The Gambia',name), name := 'Gambia']
ldi[grep('Bosnia',name), name := 'Bosnia & Herzegovina']
ldi[grep('Russian Federation',name), name := 'Russia']
ldi[grep('Federated States of Micronesia',name), name := 'Micronesia']
mig[!(country_orig %in% ldi[, name]), unique(country_orig)]
ldi <- ldi[, c('name','year','ldi_pc')]
setnames(ldi, 'name', 'country')
saveRDS(ldi, 'C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/ldi.RDS')

## Add GBD shocks mortality rate per 1000
shocks <- fread('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/gbd_mx_shocks.csv')
setnames(shocks, 'mean_value', 'gbd_mx_shocks')
setnames(shocks, 'year_id', 'year')
setnames(shocks, 'location_id', 'loc_id')
setnames(shocks, 'location_name', 'name')
for(y in seq(1950,2015,5)) shocks[year>=y, five_year := y]
shocks <- shocks[, list(gbd_mx_shocks=mean(gbd_mx_shocks)), by=c('name','five_year')]
setnames(shocks, 'five_year', 'year')
shocks <- shocks[, c('name','year','gbd_mx_shocks')]
shocks[, gbd_mx_shocks := log((gbd_mx_shocks * 1000) + 0.000001)]
mig[!(country_orig %in% shocks[, name]), unique(country_orig)]
shocks[grep('Ivoire',name), name := 'Ivory Coast']
shocks[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
shocks[grep('The Gambia',name), name := 'Gambia']
shocks[grep('Bosnia',name), name := 'Bosnia & Herzegovina']
shocks[grep('Russian Federation',name), name := 'Russia']
shocks[grep('Federated States of Micronesia',name), name := 'Micronesia']
mig[!(country_orig %in% shocks[, name]), unique(country_orig)]
setnames(shocks, 'name', 'country')
saveRDS(shocks, 'C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/shocks.RDS')

## Load GBD education
edu <- fread('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/edu_15_24.csv')
setnames(edu,'location_name','name')
mig[!(country_orig %in% edu[, name]), unique(country_orig)]
edu[grep('Ivoire',name), name := 'Ivory Coast']
edu[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
edu[grep('The Gambia',name), name := 'Gambia']
edu[grep('Bosnia',name), name := 'Bosnia & Herzegovina']
edu[grep('Russian Federation',name), name := 'Russia']
edu[grep('Federated States of Micronesia',name), name := 'Micronesia']
edu[grep('Bolivia',name), name := 'Bolivia']
edu[grep('Tanzania',name), name := 'Tanzania']
edu[grep('Democratic Republic of Congo',name), name := 'DR Congo']
edu[grep('Cabo Verde',name), name := 'Cape Verde']
edu[grep('Czech',name), name := 'Czech Republic']
edu[grep('Syria',name), name := 'Syria']
edu[grep('Lao',name), name := 'Laos']
edu[grep('Viet',name), name := 'Vietnam']
edu[grep('Iran',name), name := 'Iran']
edu[grep('Venezuela',name), name := 'Venezuela']
edu[grep('United States',name), name := 'United States']
mig[!(country_orig %in% edu[, name]), unique(country_orig)]
setnames(edu, 'name', 'country')
saveRDS(edu, 'C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/edu.RDS')

## Load urbanicity
urban <- fread("C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/WUP2018-F16-Percentage_Total_in_Cities.csv", header = TRUE)
setnames(urban, 'Country or area','name')
urban <- melt(urban, id.vars = c('name','City Code'), measure.vars = c("1990","1995","2000","2005"), variable.name = 'year')
urban <- urban[, list(urbanicity=sum(value)), by=c('name','year')]
urban[, year := as.numeric(as.character(year))]
mig[!(country_orig %in% urban[, name]), unique(country_orig)]
urban[grep('Eswa',name), unique(name)]
urban[grep('Ivoire',name), name := 'Ivory Coast']
urban[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
urban[grep('The Gambia',name), name := 'Gambia']
urban[grep('Bosnia',name), name := 'Bosnia & Herzegovina']
urban[grep('Russian Federation',name), name := 'Russia']
urban[grep('Federated States of Micronesia',name), name := 'Micronesia']
urban[grep('Bolivia',name), name := 'Bolivia']
urban[grep('Tanzania',name), name := 'Tanzania']
urban[grep('Syria',name), name := 'Syria']
urban[grep('Iran',name), name := 'Iran']
urban[grep('Bhutan',name), name := 'Bhutan']
urban[grep('Republic of Korea',name), name := 'South Korea']
urban[grep('Veit',name), name := 'Vietnam']
urban[grep("Dem. People's Republic of Korea",name), name := 'North Korea']
urban[grep('Lao',name), name := 'Laos']
urban[grep('Brunei',name), name := 'Brunei']
urban[grep('Belize',name), name := 'Belize']
urban[grep('Bolivia',name), name := 'Bolivia']
urban[grep('Venezuela',name), name := 'Venezuela']
mig[!(country_orig %in% urban[, name]), unique(country_orig)]
setnames(urban, 'name', 'country')
saveRDS(urban, 'C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/urban.RDS')

## Merge EPR 
# ilo_epr <- fread('C:/Users/ngraetz/Downloads/ILOSTAT_epr.csv')
ilo_epr <- fread("C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/API_SL.EMP.1524.SP.FE.ZS_DS2_en_csv_v2_45173.csv", header = TRUE)
ilo_epr <- melt(ilo_epr, id.vars = 'Country Name', measure.vars = as.character(1990:2010), variable.name = 'year', value.name = 'epr_15_24')
setnames(ilo_epr, 'Country Name', 'name')
ilo_epr[, year := as.numeric(as.character(year))]
ilo_epr_1990 <- ilo_epr[year==1991, ]
ilo_epr_1990[, year := 1990]
ilo_epr <- rbind(ilo_epr[year!=1990, ], ilo_epr_1990)
mig[!(country_orig %in% ilo_epr[, name]), unique(country_orig)]
ilo_epr[grep('Korea',name), unique(name)]
ilo_epr[grep('Ivoire',name), name := 'Ivory Coast']
ilo_epr[grep('Congo, Dem. Rep.',name), name := 'DR Congo']
ilo_epr[grep('Congo, Rep.',name), name := 'Congo']
ilo_epr[grep('Cabo Verde',name), name := 'Cape Verde']
ilo_epr[grep('Egypt',name), name := 'Egypt']
ilo_epr[grep('Gambia',name), name := 'Gambia']
ilo_epr[grep('Eswa',name), name := 'Swaziland']
ilo_epr[grep('Bosnia',name), name := 'Bosnia & Herzegovina']
ilo_epr[grep('Yemen',name), name := 'Yemen']
ilo_epr[grep('Iran',name), name := 'Iran']
ilo_epr[grep('Korea, Rep',name), name := 'South Korea']
ilo_epr[grep('Korea, Dem',name), name := 'North Korea'] ## Dem. People's Republic of Korea
ilo_epr[grep('Macedonia',name), name := 'Macedonia']
ilo_epr[grep('Slovakia',name), name := 'Slovakia']
ilo_epr[grep('Kyrgyzstan',name), name := 'Kyrgyzstan']
ilo_epr[grep('Russia',name), name := 'Russia']
ilo_epr[grep('Syria',name), name := 'Syria']
ilo_epr[grep('Brunei',name), name := 'Brunei']
ilo_epr[grep('Lao',name), name := 'Laos']
ilo_epr[grep('Venezuela',name), name := 'Venezuela']
mig[!(country_orig %in% ilo_epr[, name]), unique(country_orig)]
setnames(ilo_epr, 'name', 'country')
saveRDS(ilo_epr, 'C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/epr.RDS')

## Merge polity2
polity <- fread("C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/polity2.csv")
polity <- polity[year %in% c(1990,1995,2000,2005), c('country','year','polity2')]
mig[!(country_orig %in% polity[, country]), unique(country_orig)]
polity[grep('Belize',country), unique(country)]
polity[grep('Congo Kinshasa',country), country := 'DR Congo']
polity[grep('Congo Brazzaville',country), country := 'Congo']
polity[grep('Bosnia',country), country := 'Bosnia & Herzegovina']
polity[grep('Congo Kinshasa',country), country := 'Bosnia & Herzegovina']
polity[grep('Korea South',country), country := 'South Korea']
polity[grep('Korea North',country), country := 'North Korea']
polity[grep('Myanmar',country), country := 'Myanmar']
mig[!(country_orig %in% polity[, country]), unique(country_orig)]
saveRDS(polity, 'C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/polity2.RDS')



## Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate)
# ilo_unemp <- fread("C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/API_SL.EMP.1524.SP.FE.ZS_DS2_en_csv_v2_45173.csv", header = TRUE)
ilo_unemp <- melt(ilo_unemp, id.vars = 'Country Name', measure.vars = as.character(1990:2010), variable.name = 'year', value.name = 'unemp_15_24')
setnames(ilo_unemp, 'Country Name', 'name')
ilo_unemp[, year := as.numeric(as.character(year))]
ilo_unemp_1990 <- ilo_unemp[year==1991, ]
ilo_unemp_1990[, year := 1990]
ilo_unemp <- rbind(ilo_unemp[year!=1990, ], ilo_unemp_1990)

ilo_unemp[name=='Egypt, Arab Rep.', name := 'Egypt']
ilo_unemp[name=='Congo, Rep.', name := 'Congo']
ilo_unemp[name=='Gambia, The', name := 'Gambia']
ilo_unemp[name=='Kyrgyz Republic', name := 'Kyrgyzstan']
ilo_unemp[name=='Slovak Republic', name := 'Slovakia']
ilo_unemp[name=='Yemen, Rep.', name := 'Yemen']
d <- merge(d, ilo_unemp, by=c('name','year'), all.x=TRUE)
d[, unemp_15_24 := as.numeric(unemp_15_24)]

## Arable land
land <- fread("C:/Users/ngraetz/Downloads/FAOSTAT_data_arable_land.csv")
land <- land[, c('Year','Area','Value')]
setnames(land, c('Year','Area','Value'), c('year','name','arable_pc'))
unique(d[!(name %in% land[, name]), name])
## Copy land values for Sudan to both Sudan and South Sudan.
south_sudan <- land[name=='Sudan (former)', ]
south_sudan[, name := 'South Sudan']
land[name=='Sudan (former)', name := 'Sudan']
land <- rbind(land, south_sudan)
unique(d[!(name %in% land[, name]), name])
d <- merge(d, land, by=c('name','year'), all.x=TRUE)
d[, arable_pc := (arable_pc * 1000) / total_pop]
d[, log_arable_pc := log(arable_pc)]
