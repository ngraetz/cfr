library(data.table)
library(ggplot2)
library(wpp2017)

## IHME pops/mort
source("/home/j/temp/central_comp/libraries/current/r/get_demographics.R")
source("/home/j/temp/central_comp/libraries/current/r/get_demographics_template.R")
mortality_2016_demographics <- get_demographics(gbd_team="mort", gbd_round_id=4)

pops <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/WPP2017_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.csv', skip=1)
names(pops) <- c('Index','Variant','name','Notes','country_code','year',paste0('age',as.character(c(0:79))), 'age80plus', paste0('age',as.character(c(80:100))))
broad_regions <- c('WORLD','More developed regions','Less developed regions','Least developed countries',
                   'Less developed regions, excluding least developed countries','Less developed regions, excluding China','High-income countries','Middle-income countries',
                   'Upper-middle-income countries','Lower-middle-income countries','Low-income countries','Sub-Saharan Africa','AFRICA',
                   'Eastern Africa','Middle Africa','Northern Africa','Southern Africa','Western Africa','ASIA','Eastern Asia',
                   'South-Central Asia','Central Asia','South-Eastern Asia','Western Asia','EUROPE','Eastern Europe',
                   'Northern Europe','Western Europe','Southern Europe','LATIN AMERICA AND THE CARIBBEAN','Caribbean',
                   'Central America','South America','NORTHERN AMERICA','Canada','United States of America','OCEANIA','Southern Asia')
pops <- pops[!(name %in% broad_regions), ]
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
for(y in seq(1950,2015,5)) pops[year>=y, five_year := y]
pops <- pops[, list(prop_15_19=mean(prop_15_19), size_15_19=mean(size_15_19),size_20_24=mean(size_20_24), prop_10_19=mean(prop_10_19), size_10_19=mean(size_10_19)), by=c('country_code','name','five_year')]
setnames(pops, 'five_year', 'year')
pops[, ratio_15_19_20_24 := size_15_19 / size_20_24]
ggplot() + geom_line(data=pops[name=='Liberia',], aes(x=year,y=ratio_15_19_20_24)) + xlim(c(1985,2005))

nm <- copy(net_migration)
nm[, year := year - 2.5]
ni <- copy(crni)
ni[, year := year - 2.5]
om <- copy(out_migration[,c('name','year','out_rate','out_migration')])
om[, year := year - 2.5]
mx_subset <- mx[sex=='Male' & age==0, ]
mx_subset[, year := year - 2.5]
cohorts <- merge(om, pops, by=c('name','year'), all.y=TRUE)
cohorts <- merge(cohorts, nm, by=c('name','year','country_code'), all.x=TRUE)
cohorts <- merge(cohorts, mx_subset, by=c('name','year','country_code'), all.x=TRUE)
cohorts <- merge(cohorts, ni, by=c('name','year','country_code'), all.x=TRUE)
cohorts <- cohorts[order(name,year)]

## Create lags and changes
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
cohort_change <- rbindlist(lapply(unique(cohorts[, name]), create_changes,
                                  dt = cohorts,
                                  vars = c('prop_15_19','size_15_19','mx','out_rate'),
                                  change = TRUE,
                                  lag = 5))  
cohort_change <- rbindlist(lapply(unique(cohorts[, name]), create_changes,
                                  dt = cohort_change,
                                  vars = c('prop_10_19','size_10_19'),
                                  change = TRUE,
                                  lag = 10))  

## Calculate all possible lags of r for migration correlation.
for(l in c(0,5,10,15,20)) {
cohort_change <- rbindlist(lapply(unique(cohort_change[, name]), create_changes,
                                  dt = cohort_change,
                                  vars = c('r_size_15_19','r_mx','crni','net_migration','mx','r_size_10_19'),
                                  change = FALSE,
                                  lag = l))  
}

## Angola, Burundi, Burkina Faso, Congo, Honduras
write.csv(cohort_change, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/all_migration_data.csv', row.names=FALSE)
cohort_change <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/all_migration_data.csv')
cor_countries_mort <- readRDS('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/cor_countries_mort.RDS')
transition_countries <- data.table(country=cor_countries_mort)
# write.csv(transition_countries, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/transition_countries.csv', row.names = FALSE)
# transition_countries <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/transition_countries.csv')
#write.csv(transition_countries, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/model_countries.csv', row.names = FALSE)
# transition_countries <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/transition_countries.csv')
# setnames(transition_countries, 'country', 'name')
model_countries <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/model_countries.csv')
cohort_change <- merge(cohort_change, transition_countries, by='name', all.x = TRUE)

## Merge IHME net migrant data
ihme <- fread('C:/Users/ngraetz/Desktop/net_migrants.csv')
locs <- fread('C:/Users/ngraetz/Desktop/gaul_to_loc_id.csv')
setnames(locs, 'loc_name', 'name')
locs[name=='Tanzania', name := 'United Republic of Tanzania']
locs[name=='Democratic Republic of the Congo', name := 'Democratic Republic of Congo']
locs[name=='Ivory Coast', name := "Cote d'Ivoire"]
locs[name=='Iran', name := 'Iran (Islamic Republic of)']
locs[name=='Vietnam', name := 'Viet Nam']
locs[name=='Syria', name := 'Syrian Arab Republic']
locs[name=='Czech Republic', name := 'Czechia']
locs[name=='Russia', name := 'Russian Federation']
locs[name=='Bolivia', name := 'Bolivia (Plurinational State of)']
locs[name=='Venezuela', name := 'Venezuela (Bolivarian Republic of)']
locs[name=='United States', name := 'United States of America']
locs[name=='The Gambia', name := 'Gambia']
locs[name=='Laos', name := "Lao People's Democratic Republic"]
locs[name=='Cape Verde', name := 'Cabo Verde']
locs[name=='Palestine', name := 'State of Palestine']
setnames(locs, 'ihme_lc_id', 'ihme_loc_id')
setnames(ihme, 'value', 'ihme_net_migrants')
setnames(ihme, 'year_id', 'year')
ihme <- merge(ihme, locs, by='ihme_loc_id')
ihme <- ihme[, c('ihme_net_migrants','year','name')]
ihme <- ihme[, list(ihme_net_migrants=sum(ihme_net_migrants)), by=c('year','name')]
cohort_change <- merge(cohort_change, ihme, by=c('name','year'), all.x=TRUE)

# cohort_change <- rbindlist(lapply(unique(cohort_change[, name]), create_changes,
#                                   dt = cohort_change,
#                                   vars = c('ihme_net_migrants'),
#                                   change = TRUE,
#                                   lag = 5)) 
cohort_change <- merge(cohort_change, total_pops, by=c('year','name'))
cohort_change[, ihme_rate := (ihme_net_migrants / total_pop)*1000]

## Merge IHME SDI
sdi <- fread('J:/temp/ngraetz/sdi.csv')
setnames(sdi, 'mean_value', 'sdi')
setnames(sdi, 'year_id', 'year')
setnames(sdi, 'location_id', 'loc_id')
sdi <- merge(sdi, locs, by='loc_id')
sdi <- sdi[, c('sdi','year','name')]
cohort_change <- merge(cohort_change, sdi, by=c('name','year'), all.x=TRUE)

## Merge ILO % agriculture
ilo <- fread("C:/Users/ngraetz/Downloads/ILOSTAT_.csv")
ilo <- ilo[sex=='SEX_T' & classif1.label=='Broad sector: Agriculture', ]
setnames(ilo, 'ref_area', 'ihme_loc_id')
ilo <- merge(ilo, locs, by='ihme_loc_id')
setnames(ilo, 'obs_value', 'percent_agriculture')
setnames(ilo, 'time', 'year')
cohort_change <- merge(cohort_change, ilo, by=c('name','year'), all.x=TRUE)

## For each country that is growing because of reductions in infant mortality, find the lag that produces the highest correlation with out-migration rates
find_lag <- function(n, dt, v, target, type) {
  dt.n <- dt[name==n, ]
  #message(n)
  if(length(dt.n[, name])==1) return(NULL)
  if(length(dt.n[!is.na(get(target)), name])==0) return(NULL)
  if(length(dt.n[, name])!=1) {
    for(l in c(0,5,10,15,20)) {
      dt.n[, (paste0('cor',l)) := cor(dt.n[, get(paste0('lag',l,'_',v))], dt.n[, get(target)], use='complete.obs')]
    }
    if(type=='positive') dt.n[, cor := apply(.SD, 1, max, na.rm=TRUE), .SDcols=grep("^cor", names(dt.n))]
    if(type=='negative') dt.n[, cor := apply(.SD, 1, min, na.rm=TRUE), .SDcols=grep("^cor", names(dt.n))]
    for(l in c(0,5,10,15,20)) dt.n[get(paste0('cor',l))==cor, bestlag := l]
    dt.n <- unique(dt.n[, c('name','cor','bestlag')])
    return(dt.n)
  }
}
this_v <- 'r_size_15_19'
net_cors <- rbindlist(lapply(unique(cohort_change[name %in% cor_countries_mort, name]), find_lag, dt=cohort_change, v=this_v, target='net_migration', type='negative'))
setnames(net_cors, c('cor','bestlag'), c('net_cor','net_lag'))
out_cors <- rbindlist(lapply(unique(cohort_change[name %in% cor_countries_mort, name]), find_lag, dt=cohort_change, v=this_v, target='out_rate', type='positive'))
setnames(out_cors, c('cor','bestlag'), c('out_cor','out_lag'))
ihme_cors <- rbindlist(lapply(unique(cohort_change[name %in% cor_countries_mort, name]), find_lag, dt=cohort_change, v=this_v, target='ihme_rate', type='negative'))
setnames(ihme_cors, c('cor','bestlag'), c('ihme_cor','ihme_lag'))
all_cors <- merge(net_cors, out_cors, by='name')
all_cors <- merge(all_cors, ihme_cors, by='name')


## STAGE 1: Test mortality correlations
get_cor <- function(n, dt, vars) {
  dt.n <- dt[name==n & year>=1970, ]
  dt.n <- dt.n[!is.na(get(vars[1])) & !is.na(get(vars[2])), ]
  c <- cor(dt.n[, get(vars[1])], dt.n[, get(vars[2])])
  dt.n[, corr := c]
  dt.n <- unique(dt.n[, c('name','corr')])
  return(dt.n)
}
mort_cors <- rbindlist(lapply(unique(cohort_change[, name]),get_cor,cohort_change,c('lag15_r_mx','r_size_15_19')))
mort_cors <- mort_cors[order(-corr)] ## 64 countries where correlation <= -0.4
## Try correlating MAGR 1975-1990 with MAGR in the size of the 15-19 cohorts 1990-2005 by country.
global_mort_cors <- cohort_change[year %in% c(1990,1995,2000,2005), list(mx_magr=mean(lag15_mx), size_15_19_magr=mean(r_size_15_19)), by='name']
global_mort_cors <- global_mort_cors[order(-mx_magr)] ## 64 countries where correlation <= -0.4
ggplot() +
  geom_point(data=gbd,
             aes(x=lag15_mx,
                 y=r_size_15_19))

## Try GLMs
model_data <- copy(cohort_change)
model_data[, lag0_r_size_15_19 := lag0_r_size_15_19*100]
model_data[, lag5_r_size_15_19 := lag5_r_size_15_19*100]
model_data[, lag10_r_size_15_19 := lag10_r_size_15_19*100]
model_data[, lag15_r_size_15_19 := lag15_r_size_15_19*100]
model_data[, lag20_r_size_15_19 := lag20_r_size_15_19*100]
model_data[, lag20_r_mx := lag20_r_mx*100]
model_data[, lag15_r_mx := lag15_r_mx*100]
model_data[, lag10_r_size_10_19 := lag10_r_size_10_19*100]
fit_glm <- glm(cbind(round(out_migration), round(total_pop-out_migration)) ~ 
                 lag5_r_size_15_19 + lag5_out_rate + as.factor(name) + year,
               data=model_data[name %in% cor_countries_mort & !is.na(out_migration), ], family=binomial)
glm_coefs1 <- data.table(model='GLM1',
                         name=names(fit_glm$coefficients),
                         coef=fit_glm$coefficients,
                         se=coef(summary(fit_glm))[,2],
                         p=coef(summary(fit_glm))[,4])
glm_coefs1[, coef := exp(coef)]
glm_coefs1[1:5, ]
fit_lm <- lm(out_rate ~ lag5_r_size_15_19 + lag5_out_rate + as.factor(name) + year,
             data=model_data[name %in% cor_countries_mort & !is.na(out_migration), ])
lm1 <- data.table(model='LM1',
                         name=names(fit_lm$coefficients),
                         coef=fit_lm$coefficients,
                         se=coef(summary(fit_lm))[,2],
                         p=coef(summary(fit_lm))[,4])
fit_lm <- lm(out_rate ~ lag20_r_mx + lag5_out_rate + as.factor(name) + year,
             data=model_data[name %in% cor_countries_mort & !is.na(out_migration), ])
lm2 <- data.table(model='LM2',
                  name=names(fit_lm$coefficients),
                  coef=fit_lm$coefficients,
                  se=coef(summary(fit_lm))[,2],
                  p=coef(summary(fit_lm))[,4])
fit_lm <- lm(net_migration ~ lag20_crni + lag5_net_migration + as.factor(name) + year,
             data=model_data[name %in% cor_countries_mort & !is.na(out_migration), ])
lm3 <- data.table(model='LM3',
                  name=names(fit_lm$coefficients),
                  coef=fit_lm$coefficients,
                  se=coef(summary(fit_lm))[,2],
                  p=coef(summary(fit_lm))[,4])
fit_lm <- lm(out_rate ~ lag20_crni + lag5_out_rate + as.factor(name) + year,
             data=model_data[name %in% cor_countries_mort & !is.na(out_migration), ])
lm4 <- data.table(model='LM4',
                  name=names(fit_lm$coefficients),
                  coef=fit_lm$coefficients,
                  se=coef(summary(fit_lm))[,2],
                  p=coef(summary(fit_lm))[,4])
all_lms <- rbind(glm_coefs1, lm1, lm2, lm3, lm4)
saveRDS(all_lms, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/lm_fits.RDS')


get_coefs <- function(reg='', global=FALSE) {
  if(global==TRUE) fit_lm <- lm(net_migration ~ lag5_r_size_15_19 + percent_agriculture + lag5_net_migration + as.factor(name) + year,
               data=model_data[!is.na(transition_region), ])
  if(global!=TRUE) fit_lm <- lm(net_migration ~ lag5_r_size_15_19 + percent_agriculture + lag5_net_migration + as.factor(name) + year,
               data=model_data[transition_region == reg, ])
  coefs <- data.table(model=ifelse(global,'global',reg),
                    name=names(fit_lm$coefficients),
                    coef=fit_lm$coefficients,
                    se=coef(summary(fit_lm))[,2],
                    p=coef(summary(fit_lm))[,4])
  return(coefs)
}
get_coefs_glm <- function(reg='', global=FALSE) {
  if(global==TRUE) fit_lm <- glm(cbind(round(out_migration), round(total_pop-out_migration)) ~ 
                                   lag5_r_size_15_19 + percent_agriculture + lag5_out_rate + as.factor(name) + year,
                                 data=model_data[!is.na(transition_region), ], family=binomial)
  if(global!=TRUE) fit_lm <- glm(cbind(round(out_migration), round(total_pop-out_migration)) ~ 
                                   lag5_r_size_15_19 + percent_agriculture + lag5_out_rate + as.factor(name) + year,
                                 data=model_data[transition_region == reg], family=binomial)
  coefs <- data.table(model=ifelse(global,'global',reg),
                      name=names(fit_lm$coefficients),
                      coef=exp(fit_lm$coefficients),
                      se=coef(summary(fit_lm))[,2],
                      p=coef(summary(fit_lm))[,4])
  return(coefs)
}
all_lms <- rbind(get_coefs(global=TRUE),
                 get_coefs(reg='ssa'),
                 get_coefs(reg='ea'),
                 get_coefs(reg='ca'),
                 get_coefs(reg='name'))
saveRDS(all_lms, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/lm_fits_net_cohort.RDS')
all_lms <- rbind(get_coefs_glm(global=TRUE),
                 get_coefs_glm(reg='ssa'),
                 get_coefs_glm(reg='ea'),
                 get_coefs_glm(reg='ca'),
                 get_coefs_glm(reg='name'))
saveRDS(all_lms, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/glm_fits_out_cohort.RDS')



## Try random slope models
library(lme4)
sdi_groups <- model_data[year==2005 & name %in% model_countries[, name] & !is.na(sdi), c('name','sdi')]
for(q in rev(seq(.1,1,.9/9))) {
  sdi_q <- quantile(sdi_groups[, sdi], p=q)
  message(round(sdi_q, 3))
  sdi_groups[sdi < sdi_q, sdi_group := as.character(q)]
}
for(q in as.character(rev(seq(.1,1,.9/9)))) {
  message(paste(unique(sdi_groups[sdi_group==q, name]), collapse=' '))
}
sdi_data <- merge(model_data, sdi_groups[, c('name','sdi_group')], by='name')
sdi_data[, scaled_year := year / 1000]
sdi_data[, scaled_sdi := sdi*100]
sdi_data <- merge(sdi_data, gbd, by=c('name','year'), all.x = TRUE)
#sdi_data[, lag5_r_gbd_size_15_19 := lag5_r_gbd_size_15_19*100]
sdi_data$transition_region <- NULL
sdi_data$transition_region_detailed <- NULL
sdi_data <- merge(sdi_data, model_countries, by='name', all.x=TRUE)
saveRDS(sdi_data, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/sdi_data.rds')

## Load GBD data
gbd_mort <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/gbd_u5m.csv')
setnames(gbd_mort, 'location_name', 'name')
gbd_pop <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/gbd_pops.csv')
setnames(gbd_pop, 'location_name', 'name')
gbd_15_19 <- gbd_pop[age_group_name=="15 to 19", list(gbd_size_15_19=sum(gbd_pops)), by=c('year','name')]
gbd_working_age <- gbd_pop[age_group_name %in% c("15 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59"), list(gbd_size_15_60=sum(gbd_pops)), by=c('year','name')]
#gbd_0_5 <- gbd_pop[age_group_name %in% c("Early Neonatal","Late Neonatal","Post Neonatal","1 to 4"), list(gbd_size_0_5=sum(gbd_pops)), by=c('year','name')]
gbd_pop <- merge(gbd_15_19, gbd_working_age, by=c('year','name'))
#gbd_pop <- merge(gbd_pop, gbd_0_5, by=c('year','name'))
gbd_pop[, gbd_propwa_15_19 := gbd_size_15_19/gbd_size_15_60]
## Make changes and lags
gbd_pop <- rbindlist(lapply(unique(gbd_pop[, name]), create_changes,
                                  dt = gbd_pop,
                                  vars = c('gbd_size_15_19'),
                                  change = TRUE,
                                  lag = 5,
                                  year_step = 5)) 
gbd_mort <- rbindlist(lapply(unique(gbd_mort[, name]), create_changes,
                        dt = gbd_mort,
                        vars = c('gbd_mx'),
                        change = TRUE,
                        lag = 5,
                        year_step = 5)) 
gbd <- merge(gbd_mort, gbd_pop, by=c('name','year'), all.y=TRUE)
for(l in c(0,5,10,15,20)) {
  gbd <- rbindlist(lapply(unique(gbd[, name]), create_changes,
                                    dt = gbd[!is.na(r_gbd_size_15_19)],
                                    vars = c('r_gbd_mx','r_gbd_size_15_19','gbd_mx'),
                                    change = FALSE,
                                    lag = l,
                                    year_step = 5))  
}
#gbd <- gbd[name %in% unique(sdi_data[, name]),]
gbd <- gbd[name!='Georgia']

mort_cors <- rbindlist(lapply(unique(gbd[, name]),get_cor,gbd,c('lag15_r_gbd_mx','r_gbd_size_15_19')))
mort_cors <- mort_cors[order(-corr)] 

## Try correlating MAGR 1980-1995 with MAGR in the size of the 15-19 cohorts 1995-2010 by country.
country_change <- copy(gbd[year %in% c(1980,1995), c('year','name','gbd_mx')])
country_change <- dcast(country_change, name ~ year, value.var = "gbd_mx")
country_change[, diff := `1995` - `1980`]
country_change <- country_change[order(-diff)] ## 61 countries have experienced an absolute drop in under-5 mortality greater than 0.01 between 1980-1995.
model_countries <- country_change[diff <= -.01, name]
## Countries where mortality dropped after 2000
new_countries <- copy(gbd[year %in% c(2000,2015), c('year','name','gbd_mx')])
new_countries <- dcast(new_countries, name ~ year, value.var = "gbd_mx")
new_countries[, diff := `2000` - `2015`]
new_countries <- new_countries[order(-diff)] ## 61 countries have experienced an absolute drop in under-5 mortality greater than 0.01 between 1980-1995.
new_countries <- new_countries[diff >= .01 & !(name %in% model_countries), name]
## Countries where the size of the 15-19 cohorts has significantly increased 1985-2000
size_countries <- copy(gbd[year %in% c(1985,2000), c('year','name','r_gbd_size_15_19')])
size_countries <- dcast(size_countries, name ~ year, value.var = "r_gbd_size_15_19")
size_countries[, diff := `1985` - `2000`]
size_countries <- size_countries[order(-diff)] ## 61 countries have experienced an absolute drop in under-5 mortality greater than 0.01 between 1980-1995.
size_countries <- size_countries[diff >= .01 & !(name %in% model_countries), name]

global_mort_cors <- gbd[year %in% 1995:2010 & name %in% model_countries, list(mx_magr=mean(lag15_r_gbd_mx, na.rm=TRUE), size_15_19_magr=mean(r_gbd_size_15_19)), by='name']
global_mort_cors <- global_mort_cors[order(mx_magr)] ## 64 countries where correlation <= -0.4
ggplot() +
  geom_point(data=global_mort_cors,
             aes(x=mx_magr,
                 y=size_15_19_magr))
ggplot() +
  geom_point(data=gbd[name %in% model_countries & year %in% 1995:2010,],
             aes(x=r_gbd_size_15_19,y=lag15_r_gbd_mx))
ggplot() +
  geom_line(data=gbd[name=='Uganda'],
            aes(x=year,
                y=gbd_mx))

## Fit random slope model for given IV by given category.
fit_random_slope <- function(dv, iv, cat, order_cat, zero_line, dt, country_fe=TRUE, guide_title, size_var=NULL, no_res=FALSE) {
  if(no_res==TRUE) {
    f <- as.formula(paste0(dv, ' ~ ', iv, ' + percent_agriculture + scaled_sdi + lag5_out_rate + as.factor(name) + year'))
    mixedmod <- lm(f, data=dt)
    return(list(mixedmod))
  }
  if(no_res==FALSE) {
    if(country_fe) f <- as.formula(paste0(dv, ' ~ ', iv, ' + log_lag5_out_rate + as.factor(name) + scaled_year + (1 + ', iv, ' | ', cat, ')'))
    if(!country_fe) f <- as.formula(paste0(dv, ' ~ ', iv, ' + log_lag5_out_rate + scaled_year + (1 + ', iv, ' | ', cat, ')'))
    mixedmod <- lmer(f, data=dt)
    # examine random and fixed effects
    gg.data <- data.table(slope=ranef(mixedmod)[[cat]][,iv],
                          group=rownames(ranef(mixedmod)[[cat]]))
    if(is.null(size_var)) size_var <- dv
    mean_obs <- dt[, list(outcome=mean(get(size_var), na.rm=TRUE)), by=cat]
    setnames(mean_obs, cat, 'group')
    gg.data <- merge(gg.data, mean_obs, by='group')
    ifelse(order_cat,
    gg.data[, f_group := factor(group, levels=gg.data$group[order(gg.data[, slope])])],
    gg.data[, f_group := group])
    gg <- ggplot(data=gg.data,
           aes(x=f_group, y=exp(slope+fixef(mixedmod)[iv]), size=outcome)) +
      geom_point() + 
      geom_hline(yintercept = exp(fixef(mixedmod)[iv]), color = 'red', size=2) + 
      labs(x='Grouping variable',y='Slope') + 
      theme_minimal() + 
      theme(axis.text.x = element_text(angle=60, hjust=1)) + 
      scale_size_continuous(guide = guide_legend(title = guide_title))
    if(zero_line) {
      gg <- gg + geom_hline(yintercept = 0, linetype='dashed')
    }
    return(list(gg, mixedmod))
  }
}
mod <- fit_random_slope(dv='out_rate', iv='lag5_r_gbd_size_15_19', cat='name', order_cat=TRUE, zero_line=TRUE, dt=sdi_data, country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_migration', no_res=TRUE)
summary(mod[[1]])
fixef(mod[[2]])['lag5_r_size_15_19']

sdi_data[, lag5_r_gbd_size_15_19 := lag5_r_gbd_size_15_19*100]
sdi_data[, log_out_rate := log(out_rate+0.001)]
sdi_data[, log_lag5_out_rate := log(lag5_out_rate+0.001)]
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_gbd_size_15_19', cat='name', order_cat=TRUE, zero_line=TRUE, dt=sdi_data, country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
mod[[1]]
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_gbd_size_15_19', cat='name', order_cat=TRUE, zero_line=TRUE, dt=sdi_data[!(name %in% c('Liberia','Cambodia','Tajikistan'))], country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
mod[[1]]
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_gbd_size_15_19', cat='transition_region_detailed', order_cat=TRUE, zero_line=TRUE, dt=sdi_data[!(name %in% c('Timor-Leste','Tajikistan','El Salvador','Guinea','Sierra Leone'))], guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
mod[[1]]
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_gbd_size_15_19', cat='sdi_group', order_cat=FALSE, zero_line=TRUE, dt=sdi_data[!(name %in% c('Timor-Leste','Tajikistan','El Salvador','Guinea'))], guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
mod[[1]]

out_cors <- rbindlist(lapply(unique(sdi_data[, name]), find_lag, dt=sdi_data, v='r_gbd_size_15_19', target='out_rate', type='positive'))
setnames(out_cors, c('cor','bestlag'), c('out_cor','out_lag'))


pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/stage3_v2.pdf'), width = 12, height = 6)
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_gbd_size_15_19', cat='name', order_cat=TRUE, zero_line=TRUE, dt=sdi_data, country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
mod[[1]]
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_gbd_size_15_19', cat='name', order_cat=TRUE, zero_line=TRUE, dt=sdi_data[!(name %in% c('Sierra Leone','Bhutan','South Sudan','Afghanistan'))], country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
mod[[1]]
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_gbd_size_15_19', cat='transition_region_detailed', order_cat=TRUE, zero_line=TRUE, dt=sdi_data[!(name %in% c('Sierra Leone','Bhutan','South Sudan','Afghanistan'))], guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
mod[[1]]
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_gbd_size_15_19', cat='sdi_group', order_cat=FALSE, zero_line=TRUE, dt=sdi_data[!(name %in% c('Sierra Leone','Bhutan','South Sudan','Afghanistan'))], guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
mod[[1]]
dev.off()


test <- sdi_data[!(name %in% c('Zimbabwe')) & !is.na(percent_agriculture) & year <= 2005 & !is.na(out_rate), ]
test[, lme_pred := predict(mod[[2]])]
test[, manual := 
       fixef(mod[[2]])['(Intercept)'] +
       fixef(mod[[2]])['lag5_r_size_15_19'] * lag5_r_size_15_19 +
       fixef(mod[[2]])['percent_agriculture'] * percent_agriculture +
       fixef(mod[[2]])['lag5_out_rate'] * lag5_out_rate +
       fixef(mod[[2]])['as.factor(name)Kenya'] + 
       fixef(mod[[2]])['year'] * year + 
       ranef(mod[[2]])[['transition_region_detailed']][4, 'lag5_r_size_15_19'] * lag5_r_size_15_19 + 
       ranef(mod[[2]])[['transition_region_detailed']][4, '(Intercept)']]

test[, inla_pred := inla_model$summary.fitted.values$mean]
test[, manual_inla := 
       inla_model$summary.fixed['(Intercept)',]$mean +
       inla_model$summary.fixed['lag5_r_size_15_19',]$mean * lag5_r_size_15_19 +
       inla_model$summary.fixed['percent_agriculture',]$mean * percent_agriculture +
       inla_model$summary.fixed['lag5_out_rate',]$mean * lag5_out_rate +
       inla_model$summary.fixed['as.factor(name)Kenya',]$mean +
       inla_model$summary.fixed['year',]$mean * year + 
       inla_model$summary.random$transition_region_detailed[4, ]$mean * lag5_r_size_15_19]


## Test INLA implementation and compare
dv <- 'log_out_rate'
iv <- 'lag5_r_gbd_size_15_19'
cat <- 'name'
inla_f <- as.formula(paste0(dv, ' ~ ', iv, ' + lag5_out_rate + year + f(', cat, ', ', iv, ', model = "iid")'))
inla_f <- out_rate ~ lag5_r_size_15_19 + f(transition_region_detailed, lag5_r_size_15_19, model = "iid")

inla_model =
  inla(formula=inla_f,
           data=sdi_data, family="gaussian", control.compute=list(dic=TRUE))#, control.fixed = list(prec.intercept = 0.0001))
summary(inla_model)
inla_model$summary.random

test <- sdi_data[!is.na(percent_agriculture), ]

n.block = max(sdi_data$transition_region_detailed) ## = 4
sdi_data$i.intercept = sdi_data$transition_region_detailed
sdi_data$j.intercept = sdi_data$transition_region_detailed + n.block  ## see doc for iid2d
formule_random_correlated_intercept_slope = y ~ x1 +
  f(i.intercept,  model="iid2d", n=2*n.block) +
  f(j.intercept, x1, copy="i.intercept")
model_random_correlated_intercept_slope_INLA = inla.cpo(
  formula=formule_random_correlated_intercept_slope,
  
  data=mydata, family="gaussian", control.compute=list(dic=TRUE))
summary(model_random_correlated_intercept_slope_INLA)



pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/updated_heat_maps.pdf'), width = 6, height = 10)
for(cor_var in c('net_cor','ihme_cor','out_cor')) {
heat_data <- unique(all_cors[, c('name','net_cor','out_cor','net_lag','out_lag','ihme_cor','ihme_lag')])
if(cor_var=='out_cor') heat_data[, name_f := factor(name, levels=heat_data$name[order(heat_data[, get(cor_var)])])]
if(cor_var!='out_cor') heat_data[, name_f := factor(name, levels=heat_data$name[order(-heat_data[, get(cor_var)])])]
heat_data <- melt(heat_data, id.vars = c('name_f','out_lag','net_lag','ihme_lag'), measure.vars = c('net_cor','out_cor','ihme_cor'))
heat_data[variable=='net_cor', value_lab := paste0(as.character(round(value,2)), ' (', net_lag, ')')]
heat_data[variable=='out_cor', value_lab := paste0(as.character(round(value,2)), ' (', out_lag, ')')]
heat_data[variable=='ihme_cor', value_lab := paste0(as.character(round(value,2)), ' (', ihme_lag, ')')]
heat_data[variable=='net_cor', variable := 'WPP (net)\n1950-2015']
heat_data[variable=='out_cor', variable := 'Wittgenstein (out)\n1990-2010']
heat_data[variable=='ihme_cor', variable := 'IHME (net)\n1950-2015']
heat_data[, variable := factor(variable, levels=c('WPP (net)\n1950-2015','IHME (net)\n1950-2015','Wittgenstein (out)\n1990-2010'))]
redblue <- c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac')
heat.gg <- ggplot(heat_data, aes(variable, name_f)) +
  geom_tile(aes(fill = value), color = "white") +
  geom_text(aes(label = value_lab)) + 
  #scale_fill_gradient(low = "steelblue", high = "white", na.value = "white", limits=c(-1,0)) +
  scale_fill_gradientn(colours=rev(redblue), values=c(-1,0,1), na.value = "#000000", rescaler = function(x, ...) x, oob = identity) +
  ylab("Country") +
  xlab("Comparison") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Correlation") +
  theme_minimal()
print(heat.gg)
}
dev.off()





get_cor <- function(n, dt, vars) {
  dt.n <- dt[name==n, ]
  dt.n <- dt.n[!is.na(get(vars[1])) & !is.na(get(vars[2])), ]
  c <- cor(dt.n[, get(vars[1])], dt.n[, get(vars[2])])
  dt.n[, corr := c]
  dt.n <- unique(dt.n[, c('name','corr')])
  return(dt.n)
}
cors <- rbindlist(lapply(unique(cohort_change[, name]),get_cor,cohort_change,c('out_rate','lag_age_prop')))
cors <- cors[order(corr)]
cohort_change <- merge(cohort_change, cors, by='name')





library(grid)
library(gridExtra)
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/all_trends_by_country.pdf'), width = 20, height = 8)
for(this_name in unique(sdi_data[order(name), name])) {
  message(this_name)
  mort.gg <- ggplot() +
    geom_line(data=sdi_data[name==this_name,],
              aes(x=year,
                  y=mx),
              size = 2) + 
    labs(x='Year',y='GBD: U5M') + 
    theme_minimal()
  
  rmort.gg <- ggplot() +
    geom_line(data=sdi_data[name==this_name,],
              aes(x=year,
                  y=r_mx),
              size = 2) + 
    geom_hline(yintercept = 0, color = 'red') + 
    labs(x='Year',y='GBD: growth rate in U5M') + 
    theme_minimal()
  
  age_prop.gg <- ggplot() +
    geom_line(data=sdi_data[name==this_name,],
              aes(x=year,
                  y=gbd_size_15_19/1000),
              size = 2) + 
    labs(x='Year',y='GBD: pop size 15-19 (in thousands)') + 
    theme_minimal()
  
  age_change.gg <- ggplot() +
    geom_line(data=sdi_data[name==this_name,],
              aes(x=year,
                  y=r_gbd_size_15_19),
              size = 2) + 
    geom_hline(yintercept = 0, color = 'red') + 
    labs(x='Year',y='GBD: growth rate in population size 15-19') + 
    theme_minimal()
  
  out_migration.gg <- ggplot() +
    geom_line(data=sdi_data[name==this_name,],
              aes(x=year,
                  y=out_rate),
              size = 2) + 
    labs(x='Year',y='Wittgenstein: out-migration rate (per 1000)') + 
    theme_minimal()
  
  gLegend<-function(a.plot){
    if ("ggplot" %in% class(a.plot)) {
      tmp <- ggplot_gtable(ggplot_build(a.plot))
    } else if ("grob" %in% class(a.plot)) {
      tmp <- .gplot
    }
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  #leg <- gLegend(mx.gg)
  #leg$vp <- viewport(layout.pos.row = 2:6, layout.pos.col = 13)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(6, 15)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  print(mort.gg + theme(legend.position="none"), vp = vplayout(2:6, 1:3))
  print(rmort.gg + theme(legend.position="none"), vp = vplayout(2:6, 4:6))
  print(age_prop.gg + theme(legend.position="none"), vp = vplayout(2:6, 7:9))
  print(age_change.gg + theme(legend.position="none"), vp = vplayout(2:6, 10:12))
  print(out_migration.gg + theme(legend.position="none"), vp = vplayout(2:6, 13:15))
  #grid.draw(leg)
  grid.text(this_name, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:15))
}
dev.off()

cohort_change[, moving_average := rollapplyr(data=net_migration, width=1:.N, FUN=mean, by=1), by = 'name']
cohort_change[, moving_average := lapply(.SD, rollmean, k = 3, na.pad = TRUE, partial = TRUE), by = name, .SDcols = 'net_migration']
cohort_change[name=='Nigeria', c('net_migration','moving_average')]

print('hi')
print('hi')
print('hi')
print('hi')
