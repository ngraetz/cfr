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
for(t in seq(1970,2015,5)) all_decomp <- rbind(all_decomp, rbindlist(lapply(unique(births[, name]), decomp_r, t=t)))
all_decomp[, mx_change := px_change]

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


## Look at mortality contribution to highest country-year growth rates
stage2 <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/stage2_countries.csv')
test <- copy(all_decomp[mx_change >= 0 & year >= 1990 & year <= 2010, ])
test[, country_year := paste0(name, '_', year)]
test <- test[country_year %in% stage2[, country_year]]
test[, country_year := gsub('_',', ',country_year)]
test <- test[!(name %in% c('China, Hong Kong SAR','China, Macao SAR','Western Sahara'))]
#test[, mx_change := mx_change * -1]
test <- test[abs(mx_change) >= abs(birth_change)]
test[, country_years := 1]
test_aggs <- test[, list(country_years=sum(country_years)), by='name']
## Subset to countries rather than country-years (to use country fixed effects in Stage 2 models).
test_aggs <- test_aggs[order(-country_years)]
test_aggs <- test_aggs[country_years >= 3, ]
test <- test[order(-obs_r)]
test <- test[1:50, ]
test[, country_year := factor(country_year, levels=test$country_year[order(test[, obs_r])])]
test_bar <- melt(test, id.vars=c('country_year'), measure.vars=c('mx_change','fert_change','repro_pop_change','residual_change'))
#test_bar <- melt(test, id.vars=c('country_year'), measure.vars=c('mx_change','birth_change','residual_change'))
write.csv(data.table(name=unique(test_aggs[, name])), 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/stage1_transition_countries_3.csv', row.names=FALSE)
test_bar[variable=='mx_change', variable := 'Survival']
test_bar[variable=='fert_change', variable := 'Fertility']
test_bar[variable=='repro_pop_change', variable := 'Size of reproductive\npopulation']
test_bar[variable=='residual_change', variable := 'Total age-specific net\nmigration (residual)']
test_bar[, country_year := factor(country_year, levels=test$country_year[order(test[, obs_r])])]
test[, country_year := factor(country_year, levels=test$country_year[order(test[, obs_r])])]
saveRDS(list(test,test_bar), 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/stage1_growth_vs_mort_change.RDS')

pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_1.pdf'), width = 12, height = 8)
# ggplot(data=all_decomp[mx_change <= 0 & mx_change >= -.1 & year >= 1990 & year <= 2010, ],
#        aes(x=mx_change,
#            y=obs_r)) + 
#   geom_point(size=2) +
#   geom_smooth() +
#   labs(x='Change in past mortality rates', y='Current growth rate (ages 15-19)', title='Country-years from 1990-2010') + 
#   theme_minimal()
ggplot() +
  geom_bar(data=test_bar,
           aes(x=country_year,
               y=value,
               fill=variable),
           color='black',
           stat='identity',
           width=0.7) + 
  geom_point(data=test,
             aes(x=country_year,
                 y=obs_r),
             size=3) + 
  ggtitle('Figure 1. Decomposition of the growth rate in the size of the male 15-19 age group.\nHighest 50 growth rates between 1990-2010 (5-year periods) where the mortality contribution is larger than the fertility contribution.') + 
  labs(x='Country-year',y='Growth rate in the size of the male 15-19 age group') + 
  scale_fill_manual(name = "Component\nof change", values = brewer.pal(4,'Spectral')) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
      legend.key.size = unit(2, 'lines'))
dev.off()

