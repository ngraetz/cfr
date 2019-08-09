library(data.table)
library(ggplot2)
library(sf)

## Load migration flows data
mig <- fread("C:/Users/ngraetz/Downloads/Data on the global flow of people_Version March2014.csv")
mig <- mig[country_orig!=country_dest, ]
## Collapse Hong Kong and Macao into "China".
mig[country_orig=='Hong Kong', country_orig:='China', ]
mig[country_dest=='Hong Kong', country_dest:='China', ]
mig[country_orig=='Macao', country_orig:='China', ]
mig[country_dest=='Macao', country_dest:='China', ]
mig <- mig[, lapply(.SD,sum), .SDcols=c('countryflow_1990','countryflow_1995','countryflow_2000','countryflow_2005'), by=c('country_orig','country_dest','region_orig','region_dest')]
mig_countries <- unique(mig[, country_orig])

## Create distance matrix
map <- readRDS('C:/Users/ngraetz/Desktop/admin0_map.RDS')
dist <- st_distance(st_centroid(map),st_centroid(map))
colnames(dist) <- map$ADM0_NAME
dist_dt <- as.data.table(dist)
dist_dt[, country_orig := map$ADM0_NAME]
dist_dt <- melt(dist_dt, id.vars = 'country_orig', variable.name = 'country_dest')
dist_dt[, distance := as.numeric(value) / 100000]
dist_dt <- dist_dt[country_orig != country_dest, ]
dist_dt[, value := NULL]
mig[!(country_orig %in% dist_dt[, country_orig]), unique(country_orig)]
dist_dt[grep('Ivoire',country_orig), country_orig := 'Ivory Coast']
dist_dt[grep('Ivoire',country_dest), country_dest := 'Ivory Coast']
dist_dt[grep('Democratic Republic of the Congo',country_orig), country_orig := 'DR Congo']
dist_dt[grep('Democratic Republic of the Congo',country_dest), country_dest := 'DR Congo']
dist_dt[grep('Republic of Congo',country_orig), country_orig := 'Congo']
dist_dt[grep('Republic of Congo',country_dest), country_dest := 'Congo']
dist_dt[grep('Bosnia and Herzegovina',country_orig), country_orig := 'Bosnia & Herzegovina']
dist_dt[grep('Bosnia and Herzegovina',country_dest), country_dest := 'Bosnia & Herzegovina']
dist_dt[grep('Trinidad and Tobago',country_orig), country_orig := 'Trinidad & Tobago']
dist_dt[grep('Trinidad and Tobago',country_dest), country_dest := 'Trinidad & Tobago']
dist_dt[grep('Saint Vincent and the Grenadines',country_orig), country_orig := 'Saint Vincent & Grenadines']
dist_dt[grep('Saint Vincent and the Grenadines',country_dest), country_dest := 'Saint Vincent & Grenadines']
mig[!(country_orig %in% dist_dt[, country_orig]), unique(country_orig)]

## Merge distance, melt
## Not including from migration file: Palestine, Virgin Islands, Sao Tome & Principe, Channel Islands
mig_merge <- merge(mig, dist_dt, by=c('country_orig','country_dest'), all.x=T)
mig_merge <- melt(mig_merge, id.vars=c('country_orig','country_dest','region_orig','region_dest',
                                       'distance'),
                  measure.vars = c('countryflow_1990','countryflow_1995','countryflow_2000','countryflow_2005'),
                  value.name = 'migrants')
mig_merge[, year := tstrsplit(variable,'_',keep=2)]
mig_merge[, year := as.numeric(year)]

## Merge covariates
total_pops <- readRDS('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/total_pops.RDS')
mig_merge <- merge(mig_merge, total_pops, by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)
setnames(mig_merge, 'total_pop', 'total_pop_orig')
mig_merge <- merge(mig_merge, total_pops, by.x=c('country_dest','year'), by.y=c('country','year'), all.x=T)
setnames(mig_merge, 'total_pop', 'total_pop_dest')

growth <- readRDS('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/growth.RDS')
mig_merge <- merge(mig_merge, growth, by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)

growth1524 <- readRDS('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/growth_15_24.RDS')
mig_merge <- merge(mig_merge, growth1524[, c('country','year','l.r.15.24','r.15.24')], by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)

ldi <- readRDS('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/ldi.RDS')
mig_merge <- merge(mig_merge, ldi, by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)
setnames(mig_merge, 'ldi_pc', 'ldi_pc_orig')
mig_merge <- merge(mig_merge, ldi, by.x=c('country_dest','year'), by.y=c('country','year'), all.x=T)
setnames(mig_merge, 'ldi_pc', 'ldi_pc_dest')
ldi_missing <- mig_merge[is.na(ldi_pc_orig), unique(country_orig)]

edu <- readRDS('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/edu.RDS')
mig_merge <- merge(mig_merge, edu, by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)
edu_missing <- mig_merge[is.na(edu), unique(country_orig)]

shocks <- readRDS('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/shocks.RDS')
mig_merge <- merge(mig_merge, shocks, by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)
shocks_missing <- mig_merge[is.na(gbd_mx_shocks), unique(country_orig)]

epr <- readRDS('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/epr.RDS')
mig_merge <- merge(mig_merge, epr, by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)
epr_missing <- mig_merge[is.na(epr_15_24), unique(country_orig)]

baseline_missing <- c('Channel Islands', 'Palestine', 'Reunion', 'Saint Vincent & Grenadines', 'Sao Tome & Principe', 'Virgin Islands', 'Western Sahara')
all_missing <- unique(c(ldi_missing, edu_missing, shocks_missing, epr_missing, baseline_missing))
mig_merge <- mig_merge[!(country_orig %in% all_missing) & !(country_dest %in% all_missing), ]

## Gravity model (Poisson with distance, origin population, destination population)
mig_merge[, log_ldi_pc := log(ldi_pc_orig)]
key_regions <- c('Africa','East Asia','Latin America','South-East Asia','South Asia','West Asia')
cor(mig_merge[, c('migrants','r.15.24')])

## RUN COMMENTED-OUT GRAVITY SPEC FOR EACH REGION SEPARATELY. TO-DO:
##    - Create table of contributions by country across models
##    - Generate uncertainty in predictions
##    - Generate uncertainty in the coefficient across all levels of LDI_pc
# gravity_model <- glm(migrants ~ r.15.24*log_ldi_pc*region_orig + distance + total_pop_dest + total_pop_orig + country_orig, data = mig_merge[region_orig %in% key_regions, ], family = 'poisson')
# model_data <- mig_merge[region_orig %in% key_regions, ]
# model_data[, r.15.24 := r.15.24 - mean(r.15.24)]
# model_data[, log_ldi_pc := log_ldi_pc - mean(log_ldi_pc)]
model_data <- copy(mig_merge)
model_data[, log_ldi_pc := scale(log_ldi_pc)]
model_data[, log_ldi_pc_dest := scale(log(ldi_pc_dest))]
model_data[, log_ldi_pc_dest_gap := log_ldi_pc_dest - log_ldi_pc]
model_data[, gbd_mx_shocks := scale(gbd_mx_shocks)]
model_data[, epr_15_24 := scale(epr_15_24)]
model_data[, log_distance := scale(log(distance))]
model_data[, log_pop_orig := scale(log(total_pop_orig))]
model_data[, log_pop_dest := scale(log(total_pop_dest))]

## Try only GBD regions
old_data <- readRDS("C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/paper_figs/model_data_global.RDS")
old_data <- unique(old_data[, c('name','region_f','gbd_region','urbanicity','year')])
model_data[!(country_orig %in% old_data[, name]), unique(country_orig)]
old_data[grep('Korea',name), unique(name)]
old_data[grep('Bolivia',name), name := 'Bolivia']
old_data[grep('Brunei',name), name := 'Brunei']
old_data[grep('Verde',name), name := 'Cape Verde']
old_data[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
old_data[grep('Iran',name), name := 'Iran']
old_data[grep('Ivoire',name), name := 'Ivory Coast']
old_data[grep('Lao',name), name := 'Laos']
old_data[grep("Dem. People's Republic of Korea",name), name := 'North Korea']
old_data[grep('Republic of Korea',name), name := 'South Korea']
old_data[grep('Syria',name), name := 'Syria']
old_data[grep('Tanzania',name), name := 'Tanzania']
old_data[grep('Venezuela',name), name := 'Venezuela']
old_data[grep('Viet',name), name := 'Vietnam']
setnames(old_data, 'name', 'country_orig')
## SUBSET TO COUNTRIES WE WERE USING BEFORE
model_data <- merge(model_data, old_data, by=c('country_orig','year'))
saveRDS(model_data, 'C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/paper_figs/gravity_model_data.RDS')

## Uncertainty in coefficient of growth rate by level of LDI_pc
library(MASS)
iv <- 'l.r.15.24'
outliers <- c('China','Mexico','Saudi Arabia','Kuwait','Pakistan','Iran','India')
coef_data <- copy(model_data[!(country_orig %in% outliers), ])
get_coefs <- function(r) {
  gravity_model <- glm(as.formula(paste0('migrants ~ ',iv,'*log_ldi_pc + country_orig')), data = coef_data[region_f==r,], family = 'poisson')
  gravity_model <- glm(as.formula(paste0('migrants ~ ',iv,'*log_ldi_pc + epr_15_24 + gbd_mx_shocks + log_ldi_pc_dest_gap + log_distance + log_pop_dest + log_pop_orig + country_orig')), data = coef_data[region_f==r,], family = 'poisson')
  coef(gravity_model)[!grepl('country_orig', names(coef(gravity_model)))]
  coef_table <- data.table(ldi_pc=seq(-3,3,.1))
  for(x in seq(-3,3,.1)) {
    param <- MASS::mvrnorm(1000, mu = coef(gravity_model), Sigma = vcov(gravity_model))
    param <- (param[,paste0('',iv)]) + (param[,paste0('',iv,':log_ldi_pc')] * x)
    coef_table[ldi_pc==x, growth_coef := mean(param)]
    coef_table[ldi_pc==x, growth_coef_lower := quantile(param, probs=0.025)]
    coef_table[ldi_pc==x, growth_coef_upper := quantile(param, probs=0.975)]
  }
  coef_table[, region_f := r]
  coef_data[region_f==r, pred_coef := log_ldi_pc * gravity_model$coefficients[paste0(iv,':log_ldi_pc')] + gravity_model$coefficients[iv]]
  coef_data[region_f==r, pred := exp(predict(gravity_model, coef_data[region_f==r,]))]
  # coef_data[year==1990, r.15.24.1990 := r.15.24]
  # coef_data[, r.15.24.1990 := min(r.15.24.1990, na.rm=T), by='country_orig']
  # coef_data[region_orig==r, r.15.24 := r.15.24.1990]
  coef_data[region_f==r, (iv) := 0]
  coef_data[region_f==r, pred_no_growth := exp(predict(gravity_model, coef_data[region_f==r,]))] 
  return(coef_table)
}
coef_table <- rbindlist(lapply(unique(model_data[, region_f]), get_coefs))

ggplot() +
  geom_ribbon(data=coef_table,
              aes(x=ldi_pc,
                  ymin=growth_coef_lower,
                  ymax=growth_coef_upper,
                  group=region_f),
              fill='grey',
              alpha=0.75) + 
  geom_line(data=coef_table,
            aes(x=ldi_pc,
                y=growth_coef,
                color=region_f),
            size=2) + 
  geom_jitter(data=unique(coef_data[, c('log_ldi_pc','pred_coef','region_f')]),
             aes(x=log_ldi_pc,
                 y=pred_coef,
                 fill=region_f),
             shape=21,
             alpha=0.8,
             size=5, width=0.01, height=0.01) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  lims(x=c(-2.5,2.5),y=c(-0.5,0.5)) + 
  #scale_size(name='Growth rate in 15-24 (lag-5)', breaks=c(0,2,4,6,8), range=c(1,10)) +
  scale_color_manual(name='', values=c('#253494','#2ca25f','#bdbdbd','#de2d26','#ff7f00','#ffff33'),guide=F) +
  scale_fill_manual(name='Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26','#ff7f00','#ffff33')) +
  guides(fill = guide_legend(override.aes = list(size = 10))) + 
  theme_bw()

coef_data[, diff := pred - pred_no_growth]
coef_data[diff>0, direction := 'Positive contributions']
coef_data[diff<=0, direction := 'Negative contributions']
## REGION AGGREGATES
coef_data_agg <- coef_data[, lapply(.SD,sum), .SDcols=c('migrants','pred','pred_no_growth','diff'), by=c('region_f','direction')]
coef_data_agg <- coef_data_agg[order(-diff)]
head(coef_data_agg[direction=='Positive contributions', c('region_f','migrants','pred','pred_no_growth','diff')],20)
## COUNTRY ORIGIN AGGREGATES
coef_data_agg <- coef_data[, lapply(.SD,sum), .SDcols=c('migrants','pred','pred_no_growth','diff'), by=c('country_orig','region_f','direction')]
coef_data_agg <- coef_data_agg[order(-diff)]
head(coef_data_agg[direction=='Positive contributions' & region_f %in% c('Sub-Saharan Africa','Latin America and Caribbean'), c('country_orig','region_f','migrants','pred','pred_no_growth','diff')],20)
## COUNTRY DEST AGGREGATES
coef_data_agg <- coef_data[, lapply(.SD,sum), .SDcols=c('migrants','pred','pred_no_growth','diff'), by=c('country_dest','region_f','direction')]
coef_data_agg <- coef_data_agg[order(-diff)]
head(coef_data_agg[direction=='Positive contributions' & region_f %in% c('Sub-Saharan Africa','Latin America and Caribbean'), c('country_dest','region_f','migrants','pred','pred_no_growth','diff')],20)


## Need zero-inflated Poisson (65% of bilateral flows are 0s).
gravity_model <- glm(migrants ~ l.r.15.24*log_ldi_pc + epr_15_24 + gbd_mx_shocks + log_ldi_pc_dest_gap + distance + total_pop_dest + total_pop_orig + country_orig, data = model_data[!(country_orig %in% outliers)], family = 'poisson')
coef(gravity_model)[!grepl('country_orig', names(coef(gravity_model)))]
library(boot)
library(pscl)
library(car)
## Influence plot suggests Pakistan, India, Bangladesh, and Iran are hugely influential.
influencePlot(gravity_model)
grav_zero <- zeroinfl(migrants ~ l.r.15.24*log_ldi_pc + epr_15_24 + gbd_mx_shocks + log_ldi_pc_dest_gap + log_distance + log_pop_orig + log_pop_dest + country_orig, data = model_data, dist = 'negbin')
coef(grav_zero)[!grepl('country_orig', names(coef(grav_zero)))]
influencePlot(grav_zero)


coef_table <- data.table(ldi_pc=seq(-3,3,.1))
for(x in seq(-3,3,.1)) {
  param <- MASS::mvrnorm(1000, mu = coef(gravity_model), Sigma = vcov(gravity_model))
  param <- (param[,iv]) + (param[,paste0(iv,':log_ldi_pc')] * x)
  coef_table[ldi_pc==x, growth_coef := mean(param)]
  coef_table[ldi_pc==x, growth_coef_lower := quantile(param, probs=0.025)]
  coef_table[ldi_pc==x, growth_coef_upper := quantile(param, probs=0.975)]
}
ggplot() +
  geom_ribbon(data=coef_table,
              aes(x=ldi_pc,
                  ymin=growth_coef_lower,
                  ymax=growth_coef_upper),
              fill='grey',
              alpha=0.75) + 
  geom_line(data=coef_table,
            aes(x=ldi_pc,
                y=growth_coef,
                color=region_f),
            size=2) + 
  geom_jitter(data=unique(coef_data[, c('log_ldi_pc','pred_coef','region_f')]),
              aes(x=log_ldi_pc,
                  y=pred_coef,
                  fill=region_f),
              shape=21,
              alpha=0.8,
              size=5, width=0.01, height=0.01) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  lims(x=c(-2.5,2.5),y=c(-0.5,0.5)) + 
  scale_color_manual(name='', values=c('#253494','#2ca25f','#bdbdbd','#de2d26','#ff7f00','#ffff33'),guide=F) +
  scale_fill_manual(name='Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26','#ff7f00','#ffff33')) +
  guides(fill = guide_legend(override.aes = list(size = 10))) + 
  theme_bw()

full_results <- copy(model_data[!(country_orig %in% outliers)])
full_results[, pred := exp(predict(gravity_model, full_results))]
full_results[, (iv) := 0]
full_results[, pred_no_growth := exp(predict(gravity_model, full_results))] 
full_results[, diff := pred - pred_no_growth]
full_results[diff>0, direction := 'Positive contributions']
full_results[diff<=0, direction := 'Negative contributions']
full_results_agg <- full_results[, lapply(.SD,sum), .SDcols=c('migrants','pred','pred_no_growth','diff'), by=c('country_orig','region_f','direction')]
full_results_agg[, perc := pred / pred_no_growth]
full_results_agg <- full_results_agg[order(-perc)]
head(full_results_agg[direction=='Positive contributions', c('country_orig','region_f','migrants','pred','pred_no_growth','diff','perc')],20)


test <- model_data[, list(migrants=sum(migrants),log_ldi_pc=mean(log_ldi_pc),growth=mean(l.r.15.24)), by=c('year','country_orig','region_f')]
test <- test[order(-migrants)]
# test <- test[country_orig %in% c('Pakistan', 'India', 'Bangladesh', 'Iran'), ]
pdf('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/country_tests.pdf', height=8, width=12)
# for(r in unique(test$region_f)) print(ggplot(data=test[region_f==r & !(country_orig %in% outliers),], aes(x=log_ldi_pc,y=growth)) + geom_text(aes(label=country_orig,size=migrants)))
print(ggplot(data=test[!(country_orig %in% outliers),], aes(x=log_ldi_pc,y=growth)) + geom_text(aes(label=country_orig,size=migrants)))
dev.off()

ggplot(data=model_data, aes(x=migrants,y=l.r.15.24)) + geom_point()

model_data[, zero := ifelse(migrants==0, 1, 0)]
model_data[, country_years := 1]
test <- model_data[, list(zero=sum(zero), country_years=sum(country_years)), by='country_orig']
test <- test[, zero_prop := zero / country_years]
head(test[order(-zero_prop)],20)
zero_countries <- test[zero_prop>.8, country_orig]
