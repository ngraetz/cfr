library(data.table)
library(ggplot2)
library(wpp2017)
data("popM")
data("popF")
data("mxM")
data("mxF")
mxM <- as.data.table(mxM)
mxF <- as.data.table(mxF)
popM <- as.data.table(popM)
popF <- as.data.table(popF)

crni <- fread('C:/Users/ngraetz/Documents/repos/cfr_migration/wpp2017_crni.csv')
setnames(crni, 'Country code', 'country_code')
crni <- crni[, ('Region, subregion, country or area *') := NULL]
country_codes <- fread('C:/Users/ngraetz/Documents/repos/cfr_migration/country_codes.csv')
setnames(country_codes,'Code','country_code')
out_migration <- fread('C:/Users/ngraetz/Documents/repos/cfr_migration/abel_flows2014.csv')
out_migration <- out_migration[country_orig != country_dest, ]

migration <- fread('C:/Users/ngraetz/Documents/repos/cfr_migration/wpp2017_migration_rates.csv')
setnames(migration, 'Country code', 'country_code')
migration <- migration[, ('Region, subregion, country or area *') := NULL]


index <- crni[, c('country_code','Index')]
index <- merge(index, mxM[, c('name','country_code')], by='country_code')
index <- merge(index, country_codes[,4:13], by='country_code')
index <- unique(index)

SRB <- fread("C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/WPP2017_FERT_F02_SEX_RATIO_AT_BIRTH.csv")
setnames(SRB, 'Country code', 'country_code')
SRB[, ('Region, subregion, country or area *') := NULL]
SRB <- merge(SRB, unique(index[, c('country_code','name')]), by='country_code')

births <- fread("C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/WPP2017_FERT_F01_BIRTHS_BOTH_SEXES.csv")
setnames(births, 'Country code', 'country_code')
births[, ('Region, subregion, country or area *') := NULL]
births <- merge(births, unique(index[, c('country_code','name')]), by='country_code')
for(v in names(births)[grep('-',names(births))]) {
  births[, (v) := as.numeric(gsub(' ','',get(v)))]
  #message(unique(births[is.na(get(v)), name]))
}
all_mxM <- copy(mxM)
setnames(all_mxM, names(all_mxM)[grep('-',names(all_mxM))], paste0('y',gsub('-.*','',names(all_mxM)[grep('-',names(all_mxM))])))
setnames(births, names(births)[grep('-',names(births))], paste0('y',gsub('-.*','',names(births)[grep('-',names(births))])))
setnames(SRB, names(SRB)[grep('-',names(SRB))], paste0('y',gsub('-.*','',names(SRB)[grep('-',names(SRB))])))
setnames(popM, names(popM)[!(names(popM) %in% c('country_code','name','age'))], paste0('y',names(popM)[!(names(popM) %in% c('country_code','name','age'))]))

## Try actual life tables from WPP
lt_male <- fread("C:/Users/ngraetz/Downloads/WPP2017_SA4_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.csv")
lt_male[, year := as.numeric(substr(Period,1,4))]
lt_male[, name := Country]
lt_male[grep("d'Ivoire", name), name := "Cote d'Ivoire"]

## Load fertility rates for further decomp.
fert <- fread("C:/Users/ngraetz/Downloads/WPP2017_FERT_F07_AGE_SPECIFIC_FERTILITY.csv")
setnames(fert, 'Country code', 'country_code')
fert[, ('Region, subregion, country or area *') := NULL]
fert <- merge(fert, unique(index[, c('country_code','name')]), by='country_code')
for(v in names(fert)[grep('-',names(fert))]) {
  fert[, (v) := as.numeric(gsub(' ','',get(v)))]
}
fert[, year := as.numeric(substr(Period,1,4))]
fert <- melt(fert, id.vars = c('name','year'), measure.vars = names(fert)[grep('-',names(fert))], value.name = 'asfr', variable.name = 'age')
fert[, asfr := asfr / 1000]
repro_pop <- copy(popF)
repro_pop <- melt(repro_pop, id.vars = c('name','age'), measure.vars = as.character(seq(1950,2015,5)), value.name = 'repro_pop', variable.name = 'year')
repro_pop[, year := as.numeric(as.character(year))]
fert <- merge(fert, repro_pop, by=c('name','year','age'))

## Load female pops to get 5-year reproductive population
pops <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/WPP2017_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.csv', skip=1)
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
pops <- pops[, size_15_49 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=paste0('age',15:49)]
for(y in seq(1950,2015,5)) pops[year>=y, five_year := y]
pops <- pops[, list(size_15_49=sum(size_15_49)), by=c('name','five_year')]
setnames(pops, 'five_year', 'year')
pops <- pops[, c('name','year','size_15_49')]
pops <- dcast(pops, name ~ year, value.var = 'size_15_49')
pops[grep("d'Ivoire", name), name := "Cote d'Ivoire"]

decomp_r <- function(t,c) {
  #message(paste0(c,t))
  #mxM <- all_mxM[name==c, ]
  birth_change <- 
    log((births[name==c, get(paste0('y', t-15))] - (births[name==c, get(paste0('y', t-15))] / (1+SRB[name==c, get(paste0('y', t-15))]))) / 
          (births[name==c, get(paste0('y', t-20))] - (births[name==c, get(paste0('y', t-20))] / (1+SRB[name==c, get(paste0('y', t-20))])))) / 5
  #px_1 <- prod(lt_male[name==c & year == t-15 & `Age (x)` %in% c(0,1,5,10), as.numeric(`Probability of surviving p(x,n)`)])
  #px_2 <- prod(lt_male[name==c & year == t-20 & `Age (x)` %in% c(0,1,5,10), as.numeric(`Probability of surviving p(x,n)`)])
  px_1 <- prod(lt_male[name==c & year == t-15 & `Age (x)` == 0, as.numeric(`Probability of surviving p(x,n)`)],
               lt_male[name==c & year == t-15 & `Age (x)` == 1, as.numeric(`Probability of surviving p(x,n)`)],
               lt_male[name==c & year == t-10 & `Age (x)` == 5, as.numeric(`Probability of surviving p(x,n)`)],
               lt_male[name==c & year == t-5 & `Age (x)` == 10, as.numeric(`Probability of surviving p(x,n)`)])
  px_2 <- prod(lt_male[name==c & year == t-20 & `Age (x)` == 0, as.numeric(`Probability of surviving p(x,n)`)],
               lt_male[name==c & year == t-20 & `Age (x)` == 1, as.numeric(`Probability of surviving p(x,n)`)],
               lt_male[name==c & year == t-15 & `Age (x)` == 5, as.numeric(`Probability of surviving p(x,n)`)],
               lt_male[name==c & year == t-10 & `Age (x)` == 10, as.numeric(`Probability of surviving p(x,n)`)])
  ## Try to split up birth change to change in reproductive population and fertility rates.
  fert_compare <- fert[name==c & year %in% c(t-15,t-20), ]
  fert_compare <- dcast(fert_compare, age ~ year, value.var = c('asfr','repro_pop'))
  fert_compare[, mean_asfr := (get(paste0('asfr_', t-15))+get(paste0('asfr_', t-20))) / 2]
  fert_compare[, mean_repro_pop := (get(paste0('repro_pop_', t-15))+get(paste0('repro_pop_', t-20))) / 2]
  change_repro_pop <- sum(fert_compare[, (get(paste0('repro_pop_', t-15)) - get(paste0('repro_pop_', t-20))) * mean_asfr])
  change_fertility <- sum(fert_compare[, (get(paste0('asfr_', t-15)) - get(paste0('asfr_', t-20))) * mean_repro_pop])
  fert_contribution <- abs(change_fertility) / (abs(change_fertility) + abs(change_repro_pop))
  repro_pop_contribution <- abs(change_repro_pop) / (abs(change_fertility) + abs(change_repro_pop))
  ## Get percentages and apply to change due to growth rate in births between cohorts.
  px_change <- px_1 - px_2
  px_change <- log(px_1 / px_2) / 5
  int_r <- birth_change + px_change
  obs_r <- log(popM[name==c & age=='15-19', get(paste0('y', t))] / popM[name==c & age=='15-19', get(paste0('y', t-5))]) / 5
  residual <- obs_r - int_r
  abs_pop_change <- popM[name==c & age=='15-19', get(paste0('y', t))] - popM[name==c & age=='15-19', get(paste0('y', t-5))]
  return(data.table(name = c,
                    year = t,
                    px_change = px_change,
                    birth_change = birth_change,
                    residual_change = residual,
                    decomp_birth = abs(birth_change)/(abs(birth_change) + abs(px_change) + abs(residual)),
                    decomp_mort = abs(px_change)/(abs(birth_change) + abs(px_change) + abs(residual)),
                    obs_r = obs_r,
                    int_r = int_r,
                    abs_pop_change = abs_pop_change,
                    fert_change = birth_change*fert_contribution,
                    repro_pop_change = birth_change*repro_pop_contribution
  ))
}
all_decomp <- data.table()
for(t in seq(1985,2000,5)) all_decomp <- rbind(all_decomp, rbindlist(lapply(unique(births[, name]), decomp_r, t=t)))
all_decomp[, mx_change := px_change]

decomp_r <- function(t,c,lag=20) {
  #message(paste0(c,t))
  compare <- 15 + lag
  mxM <- all_mxM[name==c, ]
  birth_change <- 
    log((births[name==c, get(paste0('y', t-15))] - (births[name==c, get(paste0('y', t-15))] / (1+SRB[name==c, get(paste0('y', t-15))]))) / 
          (births[name==c, get(paste0('y', t-compare))] - (births[name==c, get(paste0('y', t-compare))] / (1+SRB[name==c, get(paste0('y', t-compare))])))) / lag
  #px_1 <- prod(lt_male[name==c & year == t-15 & `Age (x)` %in% c(0,1,5,10), as.numeric(`Probability of surviving p(x,n)`)])
  #px_2 <- prod(lt_male[name==c & year == t-20 & `Age (x)` %in% c(0,1,5,10), as.numeric(`Probability of surviving p(x,n)`)])
  px_1 <- prod(lt_male[name==c & year == t-15 & `Age (x)` == 0, as.numeric(`Probability of surviving p(x,n)`)],
               lt_male[name==c & year == t-15 & `Age (x)` == 1, as.numeric(`Probability of surviving p(x,n)`)],
               lt_male[name==c & year == t-10 & `Age (x)` == 5, as.numeric(`Probability of surviving p(x,n)`)],
               lt_male[name==c & year == t-5 & `Age (x)` == 10, as.numeric(`Probability of surviving p(x,n)`)])
  px_2 <- prod(lt_male[name==c & year == t-compare & `Age (x)` == 0, as.numeric(`Probability of surviving p(x,n)`)],
               lt_male[name==c & year == t-compare & `Age (x)` == 1, as.numeric(`Probability of surviving p(x,n)`)],
               lt_male[name==c & year == t-compare-5 & `Age (x)` == 5, as.numeric(`Probability of surviving p(x,n)`)],
               lt_male[name==c & year == t-compare-10 & `Age (x)` == 10, as.numeric(`Probability of surviving p(x,n)`)])
  ## Try to split up birth change to change in reproductive population and fertility rates.
  # fert_compare <- fert[name==c & year %in% c(t-15,t-35,t-30), ]
  # fert_compare[, births := (repro_pop*5) * asfr]
  # fert_compare <- dcast(fert_compare, age ~ year, value.var = c('asfr','repro_pop','births'))
  # 
  births_2 <- births[name==c, get(paste0('y', t-15))] - (births[name==c, get(paste0('y', t-15))] / (1+SRB[name==c, get(paste0('y', t-15))])) 
  births_1 <- births[name==c, get(paste0('y', t-compare))] - (births[name==c, get(paste0('y', t-compare))] / (1+SRB[name==c, get(paste0('y', t-compare))])) 
  repro_pops_2 <- pops[name==c, get(as.character(t-15))]
  repro_pops_1 <- pops[name==c, get(as.character(t-compare))]
  f_2 <- births_2 / repro_pops_2
  f_1 <- births_1 / repro_pops_1
  fert_change <- log(f_2 / f_1) / lag
  repro_pop_change <- log(repro_pops_2 / repro_pops_1) / lag

  # fert_compare[, log(sum(births_1990)/sum(births_1970)) / 20]
  # sum(fert_compare[, births_1990])
  # births[name==c, y1990]
  # fert_compare <- fert_compare[, list(asfr_1970=weighted.mean(asfr_1970, repro_pop_1970),
  #                                     asfr_1990=weighted.mean(asfr_1990, repro_pop_1990),
  #                                     repro_pop_1970=sum(repro_pop_1970),
  #                                     repro_pop_1990=sum(repro_pop_1990),
  #                                     repro_pop_1975=sum(repro_pop_1975))]
  # 
  # 
  # fert_compare[, r_repro := log(get(paste0('repro_pop_', t-15)) / get(paste0('repro_pop_', t-35))) / 20]
  # fert_compare[, r_asfr := log(get(paste0('asfr_', t-15)) / get(paste0('asfr_', t-35))) / 20]
  # fert_compare[, r_birth := r_repro + r_asfr]
  # 
  # 
  # fert_compare[, mean_asfr := (get(paste0('asfr_', t-15))+get(paste0('asfr_', t-35))) / 2]
  # fert_compare[, mean_repro_pop := (get(paste0('repro_pop_', t-15))+get(paste0('repro_pop_', t-35))) / 2]
  # change_repro_pop <- sum(fert_compare[, (get(paste0('repro_pop_', t-15)) - get(paste0('repro_pop_', t-35))) * mean_asfr])
  # change_fertility <- sum(fert_compare[, (get(paste0('asfr_', t-15)) - get(paste0('asfr_', t-35))) * mean_repro_pop])
  # fert_contribution <- abs(change_fertility) / (abs(change_fertility) + abs(change_repro_pop))
  # repro_pop_contribution <- abs(change_repro_pop) / (abs(change_fertility) + abs(change_repro_pop))
  ## Get percentages and apply to change due to growth rate in births between cohorts.
  px_change <- px_1 - px_2
  px_change <- log(px_1 / px_2) / lag
  int_r <- birth_change + px_change
  obs_r <- log(popM[name==c & age=='15-19', get(paste0('y', t))] / popM[name==c & age=='15-19', get(paste0('y', t-lag))]) / lag
  residual <- obs_r - int_r
  abs_pop_change <- popM[name==c & age=='15-19', get(paste0('y', t))] - popM[name==c & age=='15-19', get(paste0('y', t-lag))]
  return(data.table(name = c,
                    year = t,
                    px_change = px_change,
                    birth_change = birth_change,
                    residual_change = residual,
                    decomp_birth = abs(birth_change)/(abs(birth_change) + abs(px_change) + abs(residual)),
                    decomp_mort = abs(px_change)/(abs(birth_change) + abs(px_change) + abs(residual)),
                    obs_r = obs_r,
                    int_r = int_r,
                    abs_pop_change = abs_pop_change,
                    fert_change = fert_change,
                    repro_pop_change = repro_pop_change,
                    # change_fertility = change_fertility,
                    # change_repro_pop = change_repro_pop,
                    px_1970 = px_2,
                    fert_1970 = f_1,
                    repro_pops_1970 = repro_pops_1,
                    births_1970 = births_1,
                    px_1990 = px_1,
                    fert_1990 = f_2,
                    repro_pops_1990 = repro_pops_2,
                    births_1990 = births_2
  ))
}
all_decomp <- data.table()
broad_regions <- c('WORLD','More developed regions','Less developed regions','Least developed countries',
                   'Less developed regions, excluding least developed countries','Less developed regions, excluding China','High-income countries','Middle-income countries',
                   'Upper-middle-income countries','Lower-middle-income countries','Low-income countries','Sub-Saharan Africa','AFRICA',
                   'Eastern Africa','Middle Africa','Northern Africa','Southern Africa','Western Africa','ASIA','Eastern Asia',
                   'South-Central Asia','Central Asia','South-Eastern Asia','Western Asia','EUROPE','Eastern Europe',
                   'Northern Europe','Western Europe','Southern Europe','LATIN AMERICA AND THE CARIBBEAN','Caribbean',
                   'Central America','South America','NORTHERN AMERICA','Canada','United States of America','OCEANIA','Southern Asia')
bad_names <- c('Canada','Curacao','Reunion','United States of America',broad_regions)
for(t in 2010) all_decomp <- rbind(all_decomp, rbindlist(lapply(unique(births[, name])[!(unique(births[, name]) %in% bad_names)], decomp_r, t=t, lag=25)))
all_decomp[, mx_change := px_change]
pop_changes <- all_decomp[, c('name','px_1970','fert_1970','repro_pops_1970','births_1970','px_1990','fert_1990','repro_pops_1990','births_1990')]
setcolorder(pop_changes, c('name','px_1970','px_1990','repro_pops_1970','repro_pops_1990','fert_1970','fert_1990','births_1970','births_1990'))
write.csv(pop_changes, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/pop_changes.csv', row.names = FALSE)

all_decomp <- data.table()
for(t in seq(1985,2000,5)) all_decomp <- rbind(all_decomp, rbindlist(lapply(unique(births[, name])[!(unique(births[, name]) %in% bad_names)], decomp_r, t=t, lag=5)))
all_decomp[, mx_change := px_change]



model_data_global <- readRDS('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/model_data_global.RDS')
model_data_global <- unique(model_data_global[, c('name','gbd_region','gbd_super_region','size_15_24','year')])
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

wb_ids <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/wb_gbd_ids.csv')
wb_categories <- unique(wb_ids[region_id==location_id, c('location_name','region_id')])
wb_ids <- merge(wb_ids, locs[, c('name','ihme_loc_id')], by='ihme_loc_id')
for(id in wb_categories[, region_id]) {
  wb_ids[region_id == id, world_bank := wb_categories[region_id==id, location_name]]
  wb_ids[region_id == id, wb_id := id]
}
wb_ids <- wb_ids[, c('name','world_bank','wb_id')]
wb_ids[, name := gsub('\\(','',name)]
wb_ids[, name := gsub('\\)','',name)]
model_data_global <- merge(model_data_global, wb_ids, by=c('name'))

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
pops <- pops[, pop0 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('age0')]
pops <- pops[, pop1 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('age1','age2','age3','age4')]
pops <- pops[, pop15_24 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=paste0('age',15:24)]
for(y in seq(1950,2010,5)) pops[year>=y, five_year := y]
pops <- pops[, list(pop0=mean(pop0), pop1=mean(pop1), pop15_24=mean(pop15_24)), by=c('name','five_year')]
setnames(pops, 'five_year', 'year')
pops_15_24 <- copy(pops)
pops <- melt(pops, id.vars = c('name','year'), measure.vars = c('pop0','pop1'), value.name = 'pop', variable.name = 'age')
pops[, age := as.numeric(gsub('pop','',age))]
pops[grep("d'Ivoire", name), name := "Cote d'Ivoire"]

pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/with_outliers_Growth_vs_mortality_', Sys.Date(), '.pdf'), width = 12, height = 8)
outlier <- TRUE
for(agg_var in c('gbd_super_region','world_bank')) { 
  for(lag in c(0)) { 
mxM_change <- copy(mxM)
setnames(mxM_change, names(mxM_change)[grep('-',names(mxM_change))], paste0('y',gsub('-.*','',names(mxM_change)[grep('-',names(mxM_change))])))
mxM_change <- melt(mxM_change, id.vars = c('name','age'), measure.vars = grep('^y',names(mxM_change)), variable.name = 'year', value.name = 'mx')
mxM_change[, year := as.numeric(gsub('y','',year))]
mxM_change <- merge(mxM_change, pops, by=c('name','year','age'))
mxM_change <- merge(mxM_change, unique(model_data_global[, c('name','gbd_super_region','gbd_region','world_bank')]), by=c('name'))
#mxM_change <- mxM_change[, list(mx=weighted.mean(mx,pop)), by=c('year',agg_var)]
mxM_change <- mxM_change[, list(mx=weighted.mean(mx,pop)), by=c('year','name',agg_var)]
mxM_change <- mxM_change[, c('year','name','mx',agg_var), with=FALSE]
mxM_change[, year := year + lag]

pops_15_24_agg <- merge(pops_15_24, unique(model_data_global[, c('name','gbd_super_region','gbd_region','world_bank')]), by=c('name'))
#pops_15_24_agg <- pops_15_24_agg[, list(pop15_24=sum(pop15_24)), by=c('year',agg_var)]
pops_15_24_agg <- pops_15_24_agg[, c('year','name','pop15_24',agg_var), with=FALSE]
mxM_change <- merge(mxM_change, pops_15_24_agg, by=c('name','year',agg_var))

mxM_change <- dcast(mxM_change, as.formula(paste0(agg_var, ' + name ~ year')), value.var = c('pop15_24','mx'))
mxM_change[, mx_change := (mx_2010 - mx_1990) * 1000]
mxM_change[, pop_change := ((pop15_24_2010 / pop15_24_1990) - 1) * 100]
mxM_change[, r_pop := log(pop15_24_2010 / pop15_24_1990) / 20]

if(agg_var=='gbd_super_region') mxM_change[, (agg_var) := factor(get(agg_var), levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]
if(agg_var=='world_bank') mxM_change[, (agg_var) := factor(get(agg_var), levels = c('World Bank High Income','World Bank Upper Middle Income','World Bank Lower Middle Income','World Bank Low Income'))]
if(outlier==TRUE) {
if(lag==0) mxM_change <- mxM_change[!(name %in% c('Qatar','Rwanda'))]
if(lag==15) mxM_change <- mxM_change[!(name %in% c('Cambodia','Qatar','United Arab Emirates','Timor-Leste'))]
}
for(plot_var in c('pop_change')) {
if(plot_var=='pop_change') plot_var_name <- 'Relative change in population aged 15-24 (%)'
if(plot_var=='r_pop') plot_var_name <- 'Growth rate in population aged 15-24 (%)'
## Label countries with mx<=-40, pop_change>=150, r_pop>=0.45
gg <- ggplot(data=mxM_change,
             aes(x=mx_change,
                 y=get(plot_var))) +
      geom_point(aes(fill=get(agg_var),
                     size=pop15_24_2010),
                 shape=21) + 
      geom_text_repel(data=mxM_change[mx_change<=-27 | pop_change>=125 | r_pop>=0.45, ],
                aes(label=name),
                point.padding=0.5) + 
      labs(y=plot_var_name, x='Change in under-5 mortality rate (per thousand)',title=paste0('Change in 15-24 population vs. child mortality (lag ', lag, '), 1990-2015.')) + 
      scale_size_continuous(name='Population 15-24\n(thousands)', breaks=c(100,250,500,1000,2500,5000), range=c(4,15)) + 
      guides(fill = guide_legend(override.aes = list(size = 10))) +
      scale_x_reverse() + 
      theme_minimal()
if(agg_var %in% c('gbd_super_region','world_bank')) gg <- gg + scale_fill_manual(name='', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) 
print(gg)
#assign(paste0('gg_',agg_var), gg)
}
}
}
grid.arrange(
  grobs = list(gg_gbd_super_region + ggtitle('A) GBD Regions'), gg_world_bank + ggtitle('B) World Bank Income Groups')),
  widths = c(1),
  layout_matrix = rbind(c(1),
                        c(2))
)

dev.off()

agg_var <- 'gbd_super_region'
lag <- 0
mxM_change <- copy(mxM)
setnames(mxM_change, names(mxM_change)[grep('-',names(mxM_change))], paste0('y',gsub('-.*','',names(mxM_change)[grep('-',names(mxM_change))])))
mxM_change <- melt(mxM_change, id.vars = c('name','age'), measure.vars = grep('^y',names(mxM_change)), variable.name = 'year', value.name = 'mx')
mxM_change[, year := as.numeric(gsub('y','',year))]
mxM_change <- merge(mxM_change, pops, by=c('name','year','age'))
mxM_change <- merge(mxM_change, unique(model_data_global[, c('name','gbd_super_region','gbd_region','world_bank')]), by=c('name'))
#mxM_change <- mxM_change[, list(mx=weighted.mean(mx,pop)), by=c('year',agg_var)]
mxM_change <- mxM_change[, list(mx=weighted.mean(mx,pop)), by=c('year','name',agg_var)]
mxM_change <- mxM_change[, c('year','name','mx',agg_var), with=FALSE]
mxM_change[, year := year + lag]

pops_15_24_agg <- merge(pops_15_24, unique(model_data_global[, c('name','gbd_super_region','gbd_region','world_bank')]), by=c('name'))
#pops_15_24_agg <- pops_15_24_agg[, list(pop15_24=sum(pop15_24)), by=c('year',agg_var)]
pops_15_24_agg <- pops_15_24_agg[, c('year','name','pop15_24',agg_var), with=FALSE]
mxM_change <- merge(mxM_change, pops_15_24_agg, by=c('name','year',agg_var))

#mxM_change[, name := get(agg_var)]
mxM_change <- rbindlist(lapply(unique(mxM_change[, name]), create_changes,
                         dt = mxM_change,
                         vars = c('pop15_24','mx'),
                         change = TRUE,
                         lag = 10)) 
library(RColorBrewer)
if(agg_var=='gbd_super_region') mxM_change[, (agg_var) := factor(get(agg_var), levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]
ggplot() + 
  geom_point(data=mxM_change[year>=1990,],
             aes(x=mx,
                 y=pop15_24,
                 fill=year),
             shape=21,
             size=10) + 
  geom_line(data=mxM_change[year>=1990,],
             aes(x=mx,
                 y=pop15_24,
                 color=get(agg_var))) + 
  scale_color_manual(name='', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) + 
  scale_fill_gradientn(name = "", colors = brewer.pal(7,'Spectral')) +
  labs(y='Total population (15-24)',x='Under-5 mortality rate (per thousand)') + 
  theme_minimal()

mxM_change_wide <- dcast(mxM_change, as.formula(paste0(agg_var, ' + name ~ year')), value.var = c('r_pop15_24','r_mx','mx'))
ggplot() + 
geom_segment(data=mxM_change_wide[!(name %in% c('Qatar','Rwanda')), ],
             aes(x=mx_1990*1000, xend=mx_2010*1000, y=r_pop15_24_1990, yend=r_pop15_24_2010, color=get(agg_var)),
             size = 2,
             arrow = arrow(length = unit(0.5, "cm"))) + 
  geom_hline(yintercept = 0) + 
  scale_color_manual(name='', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) +
  labs(y='Growth rate in population (15-24), 1990 to 2015', x='Under-5 mortality rate (per thousand), 1990 to 2015') + 
  scale_x_reverse() + 
  theme_minimal()

ggplot(data=mxM_change[year>=1980,],
       aes(x=abschange_mx,
           y=r_pop15_24)) +
  geom_point(aes(fill=get(agg_var)),
             shape=21,
             size=6) + 
  labs(y='Relative change in population aged 15-24 (%)', x='Change in under-5 mortality rate (per thousand)',title='Change in 15-24 population vs. child mortality, 1990-2010.') + 
  scale_fill_discrete(name='World Bank Income Group') + 
  theme_minimal()

# c <- 'Liberia'
# t <- 2000
# #for(c in unique(d$name)) {
# decomp_r <- function(t,c) {
# mxM <- all_mxM[name==c, ]
# birth_change <- 
#   log((births[name==c, `1990-1995`] - (births[name==c, `1990-1995`] / (1+SRB[name==c, `1990-1995`]))) / 
#      (births[name==c, `1985-1990`] - (births[name==c, `1985-1990`] / (1+SRB[name==c, `1985-1990`])))) / 5
# mx_change <- (1 * (mxM[age==0, `1990-1995`] - mxM[age==0, `1985-1990`])) + 
#               (4 * (mxM[age==1, `1990-1995`] - mxM[age==1, `1985-1990`])) + 
#               (5 * (mxM[age==5, `1995-2000`] - mxM[age==5, `1990-1995`])) + 
#               (5 * (mxM[age==10, `2000-2005`] - mxM[age==10, `1995-2000`]))
# r <- birth_change - mx_change
# abs(birth_change)/(abs(birth_change) + abs(mx_change))
# abs(mx_change)/(abs(birth_change) + abs(mx_change))
# }

## Check if int_r is lined up with obs_r calculated this way?
## Try using p(x) from WPP life tables directly.
# decomp_r <- function(t,c) {
#   #message(paste0(c,t))
#   mxM <- all_mxM[name==c, ]
#   birth_change <- 
#     log((births[name==c, get(paste0('y', t-15))] - (births[name==c, get(paste0('y', t-15))] / (1+SRB[name==c, get(paste0('y', t-15))]))) / 
#           (births[name==c, get(paste0('y', t-20))] - (births[name==c, get(paste0('y', t-20))] / (1+SRB[name==c, get(paste0('y', t-20))])))) / 5
#   mx_change <- (1 * (mxM[age==0, get(paste0('y', t-15))]  - mxM[age==0, get(paste0('y', t-20))])) + 
#                (4 * (mxM[age==1, get(paste0('y', t-15))]  - mxM[age==1, get(paste0('y', t-20))])) + 
#                (5 * (mxM[age==5, get(paste0('y', t-10))]  - mxM[age==5, get(paste0('y', t-15))])) + 
#                (5 * (mxM[age==10, get(paste0('y', t-5))] - mxM[age==10, get(paste0('y', t-10))]))
#   int_r <- birth_change - mx_change
#   obs_r <- log(popM[name==c & age=='15-19', get(paste0('y', t))] / popM[name==c & age=='15-19', get(paste0('y', t-5))]) / 5
#   residual <- obs_r - int_r
#   abs_pop_change <- popM[name==c & age=='15-19', get(paste0('y', t))] - popM[name==c & age=='15-19', get(paste0('y', t-5))]
#   return(data.table(name = c,
#                     year = t,
#                     mx_change = mx_change,
#                     birth_change = birth_change,
#                     residual_change = residual,
#                     decomp_birth = abs(birth_change)/(abs(birth_change) + abs(mx_change) + abs(residual)),
#                     decomp_mort = abs(mx_change)/(abs(birth_change) + abs(mx_change) + abs(residual)),
#                     obs_r = obs_r,
#                     int_r = int_r,
#                     abs_pop_change = abs_pop_change
#                     ))
# }
# all_decomp <- data.table()
# for(t in seq(1970,2015,5)) all_decomp <- rbind(all_decomp, rbindlist(lapply(unique(births[, name]), decomp_r, t=t)))

# all_decomp <- data.table()
# for(t in seq(1970,2015,5)) all_decomp <- rbind(all_decomp, rbindlist(lapply('Less developed regions', decomp_r, t=t)))

# pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/stage1_decomp.pdf'), width = 10, height = 20)
# all_decomp[, name_f := factor(name, levels=all_decomp$name[order(all_decomp[year==1990, decomp_mort])])]
# ggplot() + 
#   geom_tile(data=all_decomp,
#             aes(x=year, y=name_f, fill=decomp_mort)) + 
#   scale_fill_gradient(low='#f7fcf5',
#                       high='#238b45',
#                       na.value='white',
#                       limits=c(0.5,1)) + 
#   theme_minimal()
# dev.off()
# 
# 
# ggplot() + 
#   geom_line(data=all_decomp[name=='India',],
#             aes(x=year+5,
#                 y=int_r)) +
#   geom_line(data=all_decomp[name=='India',],
#             aes(x=year,
#                 y=obs_r)) +
#   #geom_hline(yintercept = 0.5, color='red') + 
#   theme_minimal()
# 
# pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/stage1_decomp_line.pdf'), width = 12, height = 8)
# ggplot() + 
#   geom_line(data=all_decomp[name=='Less developed regions',],
#             aes(x=year,
#                 y=decomp_mort)) +
#   geom_hline(yintercept = 0.5, color='red') + 
#   labs(x='Year',y='Proportion of growth attributable to change in mortality') + 
#   theme_minimal()
# test <- all_decomp[name=='Less developed regions',]
# test[, mx_change := mx_change * -1]
# test <- melt(test, id.vars=c('year'), measure.vars=c('mx_change','birth_change'))
# ggplot() +
#   geom_bar(data=test,
#            aes(x=year,
#                y=value,
#                fill=variable),
#            color='black',
#            stat='identity') + 
#   geom_point(data=all_decomp[name=='Less developed regions',],
#              aes(x=year,
#                  y=int_r),
#              size=2) + 
#   labs(x='Year',y='Growth rate in the size of the 15-19 age group') + 
#   theme_minimal()
# dev.off()



## Reference everything to time t: period-specific intrinsic growth rate decomposition to period-specific mortality
# decomp_r <- function(t,c) {
#   mxM <- all_mxM[name==c, ]
#   birth_change <- 
#     log((births[name==c, get(paste0('y', t-15))] - (births[name==c, get(paste0('y', t-15))] / (1+SRB[name==c, get(paste0('y', t-15))]))) / 
#           (births[name==c, get(paste0('y', t-20))] - (births[name==c, get(paste0('y', t-20))] / (1+SRB[name==c, get(paste0('y', t-20))])))) / 5
#   mx_change <- (1 * (mxM[age==0, get(paste0('y', t))]  - mxM[age==0, get(paste0('y', t-5))])) + 
#                (4 * (mxM[age==1, get(paste0('y', t))]  - mxM[age==1, get(paste0('y', t-5))])) + 
#                (5 * (mxM[age==5, get(paste0('y', t))]  - mxM[age==5, get(paste0('y', t-5))])) + 
#                (5 * (mxM[age==10, get(paste0('y', t))] - mxM[age==10, get(paste0('y', t-5))]))
#   r <- birth_change - mx_change
#   return(data.table(name = c,
#                     year = t,
#                     decomp_birth = abs(birth_change)/(abs(birth_change) + abs(mx_change)),
#                     decomp_mort = abs(mx_change)/(abs(birth_change) + abs(mx_change)),
#                     r = log(popM[name==c & age=='15-19', get(paste0('y', t))] / popM[name==c & age=='15-19', get(paste0('y', t-5))]) / 5
#   ))
# }
# all_decomp <- data.table()
# for(t in seq(1970,2015,5)) all_decomp <- rbind(all_decomp, rbindlist(lapply(unique(all_mxM[, name]), decomp_r, t=t)))

fig3_countries <- unique(c(fread("C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_3_relative_data.csv")[, country_f],
                           fread("C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_3_absolute_data.csv")[, country_f]))

pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/correlation_', Sys.Date(), '.pdf'), width = 12, height = 8)
ggplot(data=all_decomp[!(name %in% c('Timor-Leste','Rwanda')) & mx_change <= 0.01, ],
       aes(x=mx_change,
           y=obs_r)) + 
  geom_point() +
  geom_smooth(method='loess',span=10) + 
  labs(x='Change in survival',y='Growth rate',title='Population growth and survival change in 15-19 year-old men\nComparing the 1970-1975 and 1990-1995 birth cohorts') + 
  theme_minimal()
dev.off()

pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/by_period_Figure_1_', Sys.Date(), '_just_Fig3_countries.pdf'), width = 12, height = 8)
#for(y in c(1985,1990,1995,2000)) {
## Look at mortality contribution to highest country-year growth rates
stage2 <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/stage2_countries.csv')
stage2[, name := gsub("\\_.*","",country_year)]
#test <- copy(all_decomp[mx_change >= 0 & year >= 1990 & year <= 2010, ])
#test <- copy(all_decomp[year >= 1985 & year <= 2000, ])

#y = 2005
#test <- copy(all_decomp[year == y, ])
test <- copy(all_decomp)
test <- test[!(name %in% c('Timor-Leste','Rwanda')), ]
test[, country_year := paste0(name, '_', year)]
test <- test[name %in% stage2[, name]]
test[, country_year := gsub('_',', ',country_year)]
test <- test[!(name %in% c('China, Hong Kong SAR','China, Macao SAR','Western Sahara'))]
#test <- test[abs(mx_change) >= abs(birth_change)]
test[, country_years := 1]
test_aggs <- test[, list(country_years=sum(country_years)), by='name']
## Subset to countries rather than country-years (to use country fixed effects in Stage 2 models).
test_aggs <- test_aggs[order(-country_years)]
test_aggs <- test_aggs[country_years >= 3, ]
#test <- test[order(-obs_r)]
test <- test[order(-mx_change)]
test <- test[name %in% fig3_countries]
test <- test[1:50, ]
test[, country_year := factor(country_year, levels=test$country_year[order(test[, obs_r])])]
test_bar <- melt(test, id.vars=c('country_year','name'), measure.vars=c('mx_change','fert_change','repro_pop_change','residual_change'))
#test_bar <- melt(test, id.vars=c('country_year'), measure.vars=c('mx_change','birth_change','residual_change'))
#write.csv(data.table(name=unique(test_aggs[, name])), 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/stage1_transition_countries_3.csv', row.names=FALSE)
test_bar[variable=='mx_change', variable := 'Survival']
test_bar[variable=='fert_change', variable := 'Fertility']
test_bar[variable=='repro_pop_change', variable := 'Size of reproductive\npopulation']
test_bar[variable=='residual_change', variable := 'Net migration']
test_bar[, country_year := factor(country_year, levels=test$country_year[order(test[, obs_r])])]
test[, country_year := factor(country_year, levels=test$country_year[order(test[, obs_r])])]
test_bar[, name := factor(name, levels=test$name[order(test[, obs_r])])]
test[, name := factor(name, levels=test$name[order(test[, obs_r])])]
#saveRDS(list(test,test_bar), 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/stage1_growth_vs_mort_change.RDS')

# tests <- readRDS('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/stage1_growth_vs_mort_change.RDS')
# test <- tests[[1]]
# test_bar <- tests[[2]]
# ggplot(data=all_decomp[mx_change <= 0 & mx_change >= -.1 & year >= 1990 & year <= 2010, ],
#        aes(x=mx_change,
#            y=obs_r)) + 
#   geom_point(size=2) +
#   geom_smooth() +
#   labs(x='Change in past mortality rates', y='Current growth rate (ages 15-19)', title='Country-years from 1990-2010') + 
#   theme_minimal()
test[, size_label := 'Observed growth rate']
this_gg <- ggplot() +
  geom_bar(data=test_bar,
           aes(x=country_year,
               y=value,
               fill=variable),
           color='black',
           stat='identity',
           width=0.7) + 
  geom_point(data=test,
             aes(x=country_year,
                 y=obs_r,
                 size=size_label)) + 
  #ggtitle(paste0(y-5,'-',y,' Decomposition of the growth rate in the number of men in the 15-19 age group.')) + 
  labs(x='',y='Growth rate') + 
  scale_fill_manual(name = "Component\nof change", values = brewer.pal(4,'Spectral')) + 
  scale_size_manual(name = '', values = 3) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
      legend.key.size = unit(2, 'lines'))
print(this_gg)
#}
dev.off()
write.csv(test_bar, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_1_bars.csv', row.names = FALSE)
write.csv(test[, c('name','year','obs_r')], 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_1_dots.csv', row.names = FALSE)




stage2 <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/stage2_countries.csv')
stage2[, name := gsub("\\_.*","",country_year)]
#test <- copy(all_decomp[mx_change >= 0 & year >= 1990 & year <= 2010, ])
#test <- copy(all_decomp[year >= 1990 & year <= 2010, ])
test <- copy(all_decomp)
test <- test[!(name %in% c('Timor-Leste','Rwanda')), ]
test[, country_year := paste0(name, '_', year)]
test <- test[name %in% stage2[, name]]
test[, country_year := gsub('_',', ',country_year)]
test <- test[!(name %in% c('China, Hong Kong SAR','China, Macao SAR','Western Sahara'))]
#test <- test[abs(mx_change) >= abs(birth_change)]
order_year <- function(y, dt) {
  panel_map <- data.table(panel=c('A','B','C','D'), year_panel=c('1990-1995','1995-2000','2000-2005','2005-2010'),year=c(1990,1995,2000,2005))
  dt_obs <- dt[year==y, ]
  dt_obs <- dt_obs[order(-obs_r)]
  dt_obs <- dt_obs[1:40]
  dt_obs[, name := factor(name, levels=dt_obs$name[order(dt_obs[, obs_r])])]
  dt_obs[, size_label := 'Observed growth rate']
  dt_bar <- melt(dt_obs, id.vars=c('name','year'), measure.vars=c('mx_change','fert_change','repro_pop_change','residual_change'))
  dt_bar[variable=='mx_change', variable := 'Survival']
  dt_bar[variable=='fert_change', variable := 'Fertility']
  dt_bar[variable=='repro_pop_change', variable := 'Size of reproductive\npopulation']
  dt_bar[variable=='residual_change', variable := 'Net migration']
  dt_bar[, name := factor(name, levels=dt_obs$name[order(dt_obs[, obs_r])])]
  this_gg <- ggplot() +
    geom_bar(data=dt_bar,
             aes(x=name,
                 y=value,
                 fill=variable),
             color='black',
             stat='identity',
             width=0.7) + 
    geom_point(data=dt_obs,
               aes(x=name,
                   y=obs_r,
                   size=size_label)) + 
    ggtitle(paste0('Panel ', panel_map[year==y, panel], ': ', panel_map[year==y, year_panel])) + 
    labs(x='',y='') + 
    scale_fill_manual(name = "Component\nof change", values = brewer.pal(4,'Spectral')) + 
    scale_size_manual(name = '', values = 3) + 
    ylim(c(-0.06, 0.17)) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1), 
          legend.key.size = unit(2, 'lines'))
  return(this_gg)
}
test_bar <- lapply(c(1990,1995,2000,2005), order_year, dt=test)
gLegend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
test_bar[[5]] <- gLegend(test_bar[[1]])
for(i in 1:4) test_bar[[i]] <- test_bar[[i]] + theme(legend.position="none")
library(grid)
library(gridExtra)
# test_bar[[6]] <- textGrob(
#   "Population growth rate in the number of males (15-19 years-old) and decomposition of drivers.",
#   gp = gpar(fontsize = 20),
#   just = 'left'
# )
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_1_', Sys.Date(), '.pdf'), width = 16, height = 12)
grid.arrange(
  grobs = test_bar,
  widths = c(2,2,1),
  layout_matrix = rbind(c(1,2,5),
                        c(3,4,5))
)
dev.off()






pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Growth_vs_mortality_', Sys.Date(), '.pdf'), width = 12, height = 8)
outlier <- FALSE
mxM_change <- copy(mxM)
setnames(mxM_change, names(mxM_change)[grep('-',names(mxM_change))], paste0('y',gsub('-.*','',names(mxM_change)[grep('-',names(mxM_change))])))
mxM_change <- melt(mxM_change, id.vars = c('name','age'), measure.vars = grep('^y',names(mxM_change)), variable.name = 'year', value.name = 'mx')
mxM_change[, year := as.numeric(gsub('y','',year))]
mxM_change <- merge(mxM_change, pops, by=c('name','year','age'))
mxM_change <- merge(mxM_change, unique(model_data_global[, c('name','gbd_super_region','gbd_region','world_bank')]), by=c('name'))
#mxM_change <- mxM_change[, list(mx=weighted.mean(mx,pop)), by=c('year',agg_var)]
mxM_change <- mxM_change[, list(mx=weighted.mean(mx,pop)), by=c('year','name','gbd_super_region','world_bank')]
mxM_change <- mxM_change[, c('year','name','mx','gbd_super_region','world_bank'), with=FALSE]
mxM_change[, year := year + lag]

pops_15_24_agg <- merge(pops_15_24, unique(model_data_global[, c('name','gbd_super_region','gbd_region','world_bank')]), by=c('name'))
#pops_15_24_agg <- pops_15_24_agg[, list(pop15_24=sum(pop15_24)), by=c('year',agg_var)]
pops_15_24_agg <- pops_15_24_agg[, c('year','name','pop15_24','gbd_super_region','world_bank'), with=FALSE]
mxM_change <- merge(mxM_change, pops_15_24_agg, by=c('name','year','gbd_super_region','world_bank'))

mxM_change <- dcast(mxM_change, as.formula(paste0('gbd_super_region + world_bank + name ~ year')), value.var = c('pop15_24','mx'))
mxM_change[, mx_change := (mx_2010 - mx_1990) * 1000]
mxM_change[, pop_change := ((pop15_24_2010 / pop15_24_1990) - 1) * 100]
mxM_change[, r_pop := log(pop15_24_2010 / pop15_24_1990) / 20]

mxM_change[, gbd_super_region := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]
mxM_change[, world_bank := factor(world_bank, levels = c('World Bank High Income','World Bank Upper Middle Income','World Bank Lower Middle Income','World Bank Low Income'))]
if(outlier==TRUE) {
  if(lag==0) mxM_change <- mxM_change[!(name %in% c('Qatar','Rwanda'))]
  if(lag==15) mxM_change <- mxM_change[!(name %in% c('Cambodia','Qatar','United Arab Emirates','Timor-Leste'))]
}
plot_var <- 'pop_change'
if(plot_var=='pop_change') plot_var_name <- 'Relative change in population aged 15-24 (%)'
if(plot_var=='r_pop') plot_var_name <- 'Growth rate in population aged 15-24 (%)'
## Label countries with mx<=-40, pop_change>=150, r_pop>=0.45
mxM_change[, mx_change_plot := mx_change * -1]
mxM_change_plot <- mxM_change[!(name %in% c('Trinidad and Tobago','Qatar','United Arab Emirates')),]
mxM_change_plot[(mx_change_plot>=30 | mx_change_plot<=2 | pop_change>=111 | pop_change<=-10 | name %in% c('China','India','Zimbabwe')), name_plot := name]
mxM_change_plot[is.na(name_plot) | name %in% c('Costa Rica','Grenada','Barbados','Kuwait','Mauritius','Puerto Rico','Thailand','Malawi'), name_plot := ""]
gg <- ggplot(data=mxM_change_plot,
             aes(x=mx_change_plot,
                 y=get(plot_var))) +
  geom_point(aes(fill=world_bank,
                 size=pop15_24_2010),
             shape=21) + 
  geom_text_repel(data=mxM_change_plot,
                  aes(x=mx_change_plot,
                      y=get(plot_var),
                      label=name_plot),
                  point.padding=0.5,
                  box.padding = unit(0.75, "lines")) +
  labs(y=plot_var_name, x='Reduction in under-5 mortality rate (per thousand)',title=paste0('Change in 15-24 population vs. child mortality (lag ', lag, '), 1990-2015.')) + 
  scale_size_continuous(name='Population 15-24\n(thousands)', breaks=c(100,250,500,1000,2500,5000), range=c(4,15)) + 
  #scale_shape_manual(name='GBD Region', values = c(22,21,23,24)) + 
  scale_fill_manual(name='World Bank Income Group', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) + 
  guides(fill = guide_legend(override.aes = list(size = 10, fill = c('#253494','#2ca25f','#bdbdbd','#de2d26'), shape=21))) +
  scale_x_continuous(
    trans = "log10"
  ) + 
  theme_minimal() 
print(gg)
dev.off() 
