draws <- readRDS('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/draws.RDS')

ldi_draws <- fread("C:/Users/ngraetz/Downloads/LDIpc_20180725_draws.csv")
setnames(ldi_draws, 'year_id', 'year')
setnames(ldi_draws, 'location_id', 'loc_id')
ldi_draws <- merge(ldi_draws, locs, by='loc_id')
ldi_draws <- ldi_draws[!(name %in% c('Georgia','Distrito Federal','Shimane'))]

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
pops <- pops[, size_0_5 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('age0','age1','age2','age3','age4')]
pops <- pops[, size_15_24 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('age15','age16','age17','age18','age19','age20','age21','age22','age23','age24')]
for(y in seq(1950,2010,5)) pops[year>=y, five_year := y]
pops <- pops[, list(size_15_24=mean(size_15_24)), by=c('name','five_year')]
setnames(pops, 'five_year', 'year')

pull_proj_variant <- function(v) {
message(v)
pop_proj <- fread(paste0("C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/", v, "_WPP2017_POP_F07_2_POPULATION_BY_AGE_MALE.csv"), skip=1)
setnames(pop_proj, c('V2','V3','V6','V10','V11'), c('variant','name','year','age15_19','age20_24'))
#names(pop_proj) <- c('Index','Variant','name','Notes','country_code','year',paste0('age',as.character(c(0:79))), paste0('age',as.character(c(80:100))))
broad_regions <- c('WORLD','More developed regions','Less developed regions','Least developed countries',
                   'Less developed regions, excluding least developed countries','Less developed regions, excluding China','High-income countries','Middle-income countries',
                   'Upper-middle-income countries','Lower-middle-income countries','Low-income countries','Sub-Saharan Africa','AFRICA',
                   'Eastern Africa','Middle Africa','Northern Africa','Southern Africa','Western Africa','ASIA','Eastern Asia',
                   'South-Central Asia','Central Asia','South-Eastern Asia','Western Asia','EUROPE','Eastern Europe',
                   'Northern Europe','Western Europe','Southern Europe','LATIN AMERICA AND THE CARIBBEAN','Caribbean',
                   'Central America','South America','NORTHERN AMERICA','Canada','United States of America','OCEANIA','Southern Asia')
pop_proj <- pop_proj[!(name %in% broad_regions), ]
cols <- grep("^age", names(pop_proj), value = TRUE)
for(c in cols) {
  pop_proj[, (c) := as.numeric(gsub(' ','',get(c)))]
}
pop_proj <- pop_proj[, size_15_24 := apply(.SD, 1, sum, na.rm=TRUE), .SDcols=c('age15_19','age20_24')]
for(y in seq(2015,2040,5)) pop_proj[year>=y, five_year := y]
pop_proj <- pop_proj[, list(size_15_24=mean(size_15_24)), by=c('name','five_year')]
setnames(pop_proj, 'five_year', 'year')
pop_proj <- rbind(pops, pop_proj)
pop_proj <- pop_proj[!is.na(year), ]

pop_proj <- rbindlist(lapply(unique(pop_proj[, name]), create_changes,
                         dt = pop_proj,
                         vars = c('size_15_24'),
                         change = TRUE,
                         lag = 10))  
## Calculate all possible lags of r for migration correlation.
pop_proj <- rbindlist(lapply(unique(pop_proj[, name]), create_changes,
                           dt = pop_proj,
                           vars = c('r_size_15_24'),
                           change = FALSE,
                           lag = 5))  
pop_proj[name=="Côte d'Ivoire", name := "Cote d'Ivoire"]

## Calculate forecast of relative effect by country with uncertainty from model and LDI forecasts.
iv <- 'lag5_r_size_15_24'
int_iv <- 'log_ldi_pc'
model_data_global <- readRDS('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/model_data_global.RDS')
log_ldi_mean <- model_data_global[, mean(log_ldi_pc, na.rm=TRUE)]
growth_mean <- model_data_global[, mean(lag5_r_size_15_24, na.rm=TRUE)]
model_data_global[, (iv) := get(iv) - mean(get(iv), na.rm=TRUE)]
model_data_global[, (int_iv) := get(int_iv) - mean(get(int_iv), na.rm=TRUE)]
forecast <- merge(pop_proj, ldi_draws, by=c('name','year'))
forecast[, lag5_r_size_15_24 := (lag5_r_size_15_24 * 100) - growth_mean]
for(draw in 1:100) {
  forecast[, (paste0('log_ldipc_', draw)) := log(get(paste0('ldipc_', draw))) - log_ldi_mean]
  forecast[, (paste0('cont_interaction_',draw)) := get(iv) * (draws[, get(paste0(iv,'_',draw))] + (draws[, get(paste0(iv,':',int_iv,'_',draw))] * get(paste0('log_ldipc_', draw))))]
  forecast[, (paste0('relative_cont_',draw)) := (exp(get((paste0('cont_interaction_',draw)))) - 1) * 100]
}
forecast[, total_relative_growth_contribution_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^relative_cont_", names(forecast))]
forecast[, total_relative_growth_contribution_upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^relative_cont_", names(forecast))]
forecast[, total_relative_growth_contribution_lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^relative_cont_", names(forecast))]
forecast[, variant := v]
return(forecast)
}
variants <- c('medium_variant','high_variant','low_variant','constant_fertility','instant_replacement','momentum','constant_mortality','no_change')
forecast <- rbindlist(lapply(variants, pull_proj_variant))

test <- dcast(forecast[name==n,], name + year ~ variant, value.var = 'lag5_r_size_15_24')

library(grid)
library(gridExtra)
fig3_countries <- unique(fread("C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_3_relative_data.csv")[, country_f])
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Projections_Fig3_countries_', Sys.Date(), '.pdf'), width = 16, height = 8)
for(n in fig3_countries) {
# pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Projections_emerging_countries_', Sys.Date(), '.pdf'), width = 16, height = 8)
# for(n in forecast[total_relative_growth_contribution_lower > 0 & year==2020 & !(name %in% fig3_countries), unique(name)]) {
project_variant <- function(v) {
forecast_gg <- ggplot() + 
  geom_line(data=forecast[year>=2000 & name==n & variant==v,],
            aes(x=year,
                y=total_relative_growth_contribution_mean)) + 
  geom_ribbon(data=forecast[year>=2000 & name==n & variant==v,],
              aes(x=year,
                  ymin=total_relative_growth_contribution_lower,
                  ymax=total_relative_growth_contribution_upper),
              alpha=0.2,
              color='grey') + 
  geom_hline(yintercept = 0, color='red') + 
  labs(x='',y='Percent increase in out-migration rate',title=v) + 
  theme_minimal()
return(forecast_gg) 
}
forecast_ggs <- lapply(variants, project_variant)
ldi_gg <- ggplot() + 
  geom_line(data=ldi_draws[name==n & year>=2000,],
            aes(x=year,
                y=rt_mean)) + 
  labs(x='',y='LDI/pc',title='LDI/pc forecast (IHME)') + 
  theme_minimal()
v <- 'medium_variant'
growth_gg <- ggplot() + 
  geom_line(data=forecast[name==n & year>=2000,],
            aes(x=year,
                y=r_size_15_24,
                color=variant)) + 
  geom_vline(xintercept = 2015, color='black', size=2) + 
  labs(x='',y='Growth rate, 15-24',title=paste0('Growth rate 15-24 forecast')) + 
  guides(color=FALSE) +
  theme_minimal()
forecast_ggs[[9]] <- ldi_gg
forecast_ggs[[10]] <- growth_gg
grid.arrange(
  grobs = forecast_ggs,
  widths = c(1,1,1,1,1),
  layout_matrix = rbind(c(1,2,3,4,5),
                        c(6,7,8,9,10)),
  top=textGrob(n,gp=gpar(fontsize=20,font=3))
)
}
dev.off()

## Make scatter of 5q0 (lag 15) with growth rate in 15-24. Do periods 1985-2005 and 2015-2040.
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

## Scatter
all_mxM <- melt(all_mxM[age==0,], id.vars = 'name', measure.vars = names(all_mxM)[!(names(all_mxM) %in% c('country_code','age','name'))], value.name = 'mx', variable.name='year')
all_mxM[, year := as.numeric(gsub('y','',year))]
all_mxM <- rbindlist(lapply(unique(all_mxM[, name]), create_changes,
                             dt = all_mxM,
                             vars = 'mx',
                             change = TRUE,
                             lag = 5))  
## Calculate all possible lags of r for migration correlation.
all_mxM <- rbindlist(lapply(unique(all_mxM[, name]), create_changes,
                             dt = all_mxM,
                             vars = c('r_mx','mx'),
                             change = FALSE,
                             lag = 15)) 
scatter_data <- merge(all_mxM, pop_proj, by=c('name','year'))
scatter_data <- merge(scatter_data, model_data_global[, c('name','year','gbd_super_region')], by=c('name','year'))

ggplot() + 
  geom_point(data=scatter_data[year %in% seq(1990,2010,5),],
             aes(x=lag15_r_mx,
                 y=r_size_15_24)) + 
  theme_minimal()
