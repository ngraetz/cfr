library(data.table)
library(ggplot2)
library(lme4)
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

model_countries <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/model_countries.csv')
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

## Load formatted dataset for fitting models.
d <- readRDS('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/sdi_data.rds')
d[, log_out_rate := log(out_rate+0.001)]
d[, log_lag5_out_rate := log(lag5_out_rate+0.001)]

## Make LDI per capita growth rates
ldi <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/ldi.csv')
setnames(ldi, 'mean_value', 'ldi_pc')
setnames(ldi, 'year_id', 'year')
setnames(ldi, 'location_id', 'loc_id')
ldi <- merge(ldi, locs, by='loc_id')
ldi <- ldi[, c('ldi_pc','year','name')]
ldi <- ldi[!(name %in% c('Georgia','Distrito Federal','Shimane'))]
for(y in seq(1950,2015,5)) ldi[year>=y, five_year := y]
ldi <- ldi[, list(ldi_pc=mean(ldi_pc)), by=c('name','five_year')]
setnames(ldi, 'five_year', 'year')
ldi <- rbindlist(lapply(unique(ldi[, name]), create_changes,
                        dt = ldi,
                        vars = c('ldi_pc'),
                        change = TRUE,
                        lag = 5))  
ldi_r <- ldi[, c('name','year','r_ldi_pc')]
d <- merge(d, ldi_r, by=c('name','year'))

## Add GBD shocks mortality rate per 1000
shocks <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/gbd_mx_shocks.csv')
setnames(shocks, 'mean_value', 'gbd_mx_shocks')
setnames(shocks, 'year_id', 'year')
setnames(shocks, 'location_id', 'loc_id')
shocks <- merge(shocks, locs, by='loc_id')
shocks <- shocks[, c('gbd_mx_shocks','year','name')]
shocks <- shocks[!(name %in% c('Georgia','Distrito Federal','Shimane'))]
for(y in seq(1950,2015,5)) shocks[year>=y, five_year := y]
shocks <- shocks[, list(gbd_mx_shocks=mean(gbd_mx_shocks)), by=c('name','five_year')]
setnames(shocks, 'five_year', 'year')
shocks <- shocks[, c('name','year','gbd_mx_shocks')]
shocks[, gbd_mx_shocks := log((gbd_mx_shocks * 1000) + 0.000001)]
d <- merge(d, shocks, by=c('name','year'))

## Load GBD education
edu <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/edu_15_24.csv')
setnames(edu,'location_name','name')
d <- merge(d, edu, by=c('name','year'))

## Load urbanicity
urban <- fread("C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/WUP2018-F16-Percentage_Total_in_Cities.csv", header = TRUE)
setnames(urban, 'Country or area','name')
urban <- melt(urban, id.vars = c('name','City Code'), measure.vars = c("1990","1995","2000","2005"), variable.name = 'year')
urban <- urban[, list(urbanicity=sum(value)), by=c('name','year')]
urban[, year := as.numeric(as.character(year))]
d <- merge(d, urban, by=c('name','year'))

## Load LDI_pc gaps
ldi <- fread('C:/Users/ngraetz/Documents/repos/cfr_migration/ldi_pc.csv')
d <- merge(d, ldi, by=c('name','year'))
d[ldi_pc_gap < 0, ldi_pc_gap := 0.001]
d[, ldi_pc_gap := ldi_pc_gap / 1000]
d[, log_ldi_pc := log(ldi_pc)]
d[, percent_agriculture_1 := percent_agriculture / 100]

gbd_regs <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/gbd_regions.csv')
setnames(gbd_regs, 'location_name', 'name')
d <- merge(d, gbd_regs, by='name', all.x = TRUE)

## Rescale potential IVs
for(l in c(0,5,10,15,20)) {
  d[, (paste0('lag',l,'_r_size_15_19')) := get(paste0('lag',l,'_r_size_15_19'))*100]
  d[, (paste0('lag',l,'_r_gbd_size_15_19')) := get(paste0('lag',l,'_r_gbd_size_15_19'))*100]
  print(summary(d[, get(paste0('lag',l,'_r_size_15_19'))]))
  print(summary(d[, get(paste0('lag',l,'_r_gbd_size_15_19'))]))
}

## Merge EPR 
ilo_epr <- fread('C:/Users/ngraetz/Downloads/ILOSTAT_epr.csv')
ilo_epr <- ilo_epr[sex=='SEX_T' & classif1.label=='Age: 25+', ]
setnames(ilo_epr, 'ref_area', 'ihme_loc_id')
ilo_epr <- merge(ilo_epr, locs, by='ihme_loc_id')
setnames(ilo_epr, 'obs_value', 'epr')
setnames(ilo_epr, 'time', 'year')
ilo_epr <- ilo_epr[, c('name','year','epr')]
ilo_epr_1990 <- ilo_epr[year==1991, ]
ilo_epr_1990[, year := 1990]
ilo_epr <- rbind(ilo_epr, ilo_epr_1990)
d <- merge(d, ilo_epr, by=c('name','year'), all.x=TRUE)

## Merge polity2
polity <- fread("C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/polity2.csv")
polity <- polity[year %in% c(1990,1995,2000,2005), c('country','year','polity2')]
polity_names <- unique(polity[, country])
d_names <- unique(d[, name])
missing <- data.table(country=polity_names[!(polity_names %in% d_names)])
write.csv(missing, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/missing_names_polity2.csv', row.names=FALSE)
setnames(polity, 'country', 'name')

## Create categories by SDI and LDI
# sdi_groups <- d[year==2005 & !is.na(sdi) & name %in% model_countries[, name], c('name','sdi','ldi_pc','epr')]
# for(q in rev(seq(.1,1,.9/5))) {
#   sdi_q <- quantile(sdi_groups[, sdi], p=q)
#   ldi_q <- quantile(sdi_groups[, ldi_pc], p=q)
#   epr_q <- quantile(sdi_groups[, epr], p=q)
#   #message(round(sdi_q, 3))
#   sdi_groups[sdi <= sdi_q, sdi_group := as.character(q)]
#   sdi_groups[ldi_pc <= ldi_q, ldi_group := as.character(q)]
#   sdi_groups[epr <= epr_q, epr_group := as.character(q)]
# }
# for(q in as.character(rev(seq(.1,1,.9/5)))) {
#   #message(paste(unique(sdi_groups[sdi_group==q, name]), collapse=' '))
#   #message(paste(unique(sdi_groups[ldi_group==q, name]), collapse=' '))
#   message(paste(unique(sdi_groups[epr_group==q, name]), collapse=' '))
# }
# d <- merge(d, sdi_groups[, c('name','sdi_group','ldi_group','epr_group')], by='name')
d <- d[name!='Georgia', ]

## Merge ratios of size of 15-19 to 20-24
d[, size_15_19 := NULL]
d[, size_10_19 := NULL]
d[, r_size_15_19 := NULL]
d[, r_size_10_19 := NULL]
d[, lag0_r_size_15_19 := NULL]
d[, lag0_r_size_10_19 := NULL]
d[, lag5_r_size_15_19 := NULL]
d[, lag5_r_size_10_19 := NULL]
d <- merge(d, pops, by=c('name','year'))
for(v in c(paste0('lag0_', c('r_size_20_24','r_size_25_29','r_size_10_19','r_size_15_24','r_size_15_19','r_size_15_29')),
           paste0('lag5_', c('r_size_20_24','r_size_25_29','r_size_10_19','r_size_15_24','r_size_15_19','r_size_15_29')))) d[, (v) := get(v) * 100]
#d[, prop_15_29 := prop_15_29 * 100]
#d[, r_size_15_19 := r_size_15_19 * 100]
d[, r_ldi_pc := r_ldi_pc * 100]
d <- merge(d, polity, by=c('name','year'), all.x=TRUE)
#saveRDS(d, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/model_data.rds')

## Examine different ways of choosing model subset of countries.

## Examine choosing different growth lags affecting out-migration by quantiles of employment-population ratios ("absorpative capacity").
## Does it seem like shorter lags work better for those countries with low EPRs, and vice versa?
find_lag <- function(n, dt, v, target, type) {
  dt.n <- dt[epr_group==n, ]
  #message(n)
  if(length(dt.n[, name])==1) return(NULL)
  if(length(dt.n[!is.na(get(target)), name])==0) return(NULL)
  if(length(dt.n[, name])!=1) {
    for(l in c(0,5,10,15)) {
      dt.n[, (paste0('cor',l)) := cor(dt.n[, get(paste0('lag',l,'_',v))], dt.n[, get(target)], use='complete.obs')]
      m <- lm(paste0(target,' ~ lag',l,'_',v, ' + log_lag5_out_rate + percent_agriculture + ldi_pc_gap + as.factor(name) + as.factor(year)'), dt.n)
      dt.n[, (paste0('slope',l)) := m$coefficients[paste0('lag',l,'_',v)]]
      ggplot() + geom_point(data=dt.n, aes(x=get(paste0('lag',l,'_',v)),y=get(target)))
    }
    if(type=='positive') {
      dt.n[, cor := apply(.SD, 1, max, na.rm=TRUE), .SDcols=grep("^cor", names(dt.n))]
      dt.n[, slope := apply(.SD, 1, max, na.rm=TRUE), .SDcols=grep("^slope", names(dt.n))]
    }
    if(type=='negative') {
      dt.n[, cor := apply(.SD, 1, min, na.rm=TRUE), .SDcols=grep("^cor", names(dt.n))]
      dt.n[, slope := apply(.SD, 1, min, na.rm=TRUE), .SDcols=grep("^slope", names(dt.n))]
    }
    for(l in c(0,5,10,15)) dt.n[get(paste0('cor',l))==cor, bestlag := l]
    for(l in c(0,5,10,15)) dt.n[get(paste0('slope',l))==slope, bestslope := l]
    dt.n <- unique(dt.n[, c('epr_group','cor','slope','bestlag','bestslope')])
    return(dt.n)
  }
}
lag_cors <- rbindlist(lapply(unique(d[, epr_group]), find_lag, dt=d, v='r_size_15_19', target='log_out_rate', type='positive'))
lag_cors <- lag_cors[order(-epr_group)]
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/epr_lags.pdf'), width = 6, height = 4)
ggplot(data=lag_cors, aes(x=as.numeric(epr_group), y=bestlag)) + geom_point(aes(size=exp(slope))) + geom_line() + theme_minimal() + labs(x='Employment-population ratio quantile',y='Lag on growth rate in 15-19 cohort')
dev.off()
## Add "best lagged growth rate" variable based on these results.
for(g in unique(d[, epr_group])) {
  d[epr_group==g, best_lag_r := get(paste0('lag', lag_cors[epr_group==g, bestslope],'_r_size_15_19'))]
}

## Fit random slope model for given IV by given category.
fit_random_slope <- function(dv, iv, cat, order_cat, zero_line, dt, country_fe=TRUE, random_slope=FALSE, guide_title, size_var=NULL, other_fes='', no_res=FALSE, country_res=FALSE, plot=FALSE, model_name='') {
  if(no_res==TRUE) {
    if(country_fe) f <- as.formula(paste0(dv, ' ~ ', iv, other_fes, ' + as.factor(name)'))
    if(!country_fe) f <- as.formula(paste0(dv, ' ~ ', iv, other_fes))
    mixedmod <- lm(f, data=dt)
    ## Get coefs table
    coefs <- data.table(model=model_name,
                        name=names(mixedmod$coefficients),
                        coef=mixedmod$coefficients,
                        se=coef(summary(mixedmod))[,2],
                        p=coef(summary(mixedmod))[,4])
    return(list(mixedmod, coefs))
  }
  if(no_res==FALSE) {
    if(country_fe & random_slope) f <- as.formula(paste0(dv, ' ~ ', iv, other_fes, ' + as.factor(name) + (1 + ', iv, ' | ', cat, ')'))
    if(!country_fe & random_slope) f <- as.formula(paste0(dv, ' ~ ', iv, other_fes, ' + (1 + ', iv, ' | ', cat, ')'))
    if(country_res & !random_slope) f <- as.formula(paste0(dv, ' ~ ', iv, other_fes, ' + (1|name)'))
    if(country_res & random_slope) f <- as.formula(paste0(dv, ' ~ ', iv, other_fes, ' + (1|name) + (1 + ', iv, ' | ', cat, ')'))
    mixedmod <- lmer(f, data=dt, REML = FALSE)
    # examine random and fixed effects
    if(plot) {
      gg.data <- data.table(slope=ranef(mixedmod)[[cat]][,1],
                            group=rownames(ranef(mixedmod)[[cat]]))
      if(is.null(size_var)) size_var <- dv
      mean_obs <- dt[, list(outcome=mean(as.numeric(get(size_var)), na.rm=TRUE), epr=mean(epr, na.rm=TRUE)), by=cat]
      setnames(mean_obs, cat, 'group')
      gg.data <- merge(gg.data, mean_obs, by='group')
      ifelse(order_cat,
             gg.data[, f_group := factor(group, levels=gg.data$group[order(gg.data[, slope])])],
             gg.data[, f_group := group])
      gg <- ggplot(data=gg.data,
                   aes(x=f_group, y=slope+fixef(mixedmod)[iv], size=as.numeric(outcome))) +
        geom_point() +
        geom_hline(yintercept = fixef(mixedmod)[iv], color = 'red', size=2) +
        labs(x='Grouping variable',y='Slope') +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=60, hjust=1)) +
        scale_size_continuous(guide = guide_legend(title = guide_title))
      if(zero_line) {
        gg <- gg + geom_hline(yintercept = 0, linetype='dashed')
      }
      gg2 <- ggplot(data=gg.data,
                    aes(x=epr, y=slope+fixef(mixedmod)[iv], size=as.numeric(outcome))) +
        geom_point() +
        geom_hline(yintercept = fixef(mixedmod)[iv], color = 'red', size=2) +
        labs(x='Employment-population ratio',y='Slope') +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=60, hjust=1)) +
        scale_size_continuous(guide = guide_legend(title = guide_title))
    }
    if(!plot) {
      gg.data <- NULL
      gg <- NULL
      gg2 <- NULL
    }
    ## Get coefs table
    pcoefs <- data.frame(coef(summary(mixedmod)))
    p <- 2 * (1 - pnorm(abs(pcoefs$t.value)))
    coefs <- data.table(model=model_name,
                        name=names(fixef(mixedmod)),
                        coef=fixef(mixedmod),
                        p=p)
    ranef <- data.table(model=model_name,
                        name=rownames(ranef(mixedmod)[[cat]]),
                        coef=ranef(mixedmod)[[cat]][,1],
                        p=rep(1, length(model_name)))
    coefs <- rbind(coefs, ranef)
    return(list(gg, mixedmod, coefs, gg2, gg.data))
  }
}
## Make H&W classic linear models, look at universal beta for 15-19 growth rates on out-migration rates.
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_size_15_19', cat='name', no_res=TRUE, order_cat=TRUE, zero_line=TRUE, dt=d, country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
summary(mod[[1]])
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_gbd_size_15_19', cat='name', no_res=TRUE, order_cat=TRUE, zero_line=TRUE, dt=d, country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
summary(mod[[1]])
## Use best lagged growth rate.
mod <- fit_random_slope(model_name='linear', dv='log_out_rate', iv='best_lag_r', cat='name', no_res=TRUE, order_cat=TRUE, zero_line=TRUE, dt=d, country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
summary(mod[[1]])
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_size_15_19', cat='epr_group', order_cat=FALSE, zero_line=TRUE, dt=d, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
summary(mod[[2]])
mod[[1]]

## Expand to random slope model on stratifier of interest.
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_gbd_size_15_19', cat='name', order_cat=TRUE, zero_line=TRUE, dt=d, country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
mod[[1]]
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_gbd_size_15_19', cat='name', order_cat=TRUE, zero_line=TRUE, dt=d[!(name %in% c('Liberia','Cambodia','Tajikistan'))], country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
mod[[1]]
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_gbd_size_15_19', cat='epr_group', order_cat=FALSE, zero_line=TRUE, dt=d, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
mod[[1]]
mod <- fit_random_slope(dv='log_out_rate', iv='lag5_r_size_15_19', cat='ldi_group', order_cat=FALSE, zero_line=TRUE, dt=d, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
mod[[1]]


## Paper draft models
mod <- fit_random_slope(model_name='linear', dv='log_out_rate', iv='lag5_r_size_15_19', cat='name', no_res=TRUE, order_cat=TRUE, zero_line=TRUE, dt=d, country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
saveRDS(mod[[2]], 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/hw_fit.RDS')
mod <- fit_random_slope(model_name='random', dv='log_out_rate', iv='lag5_r_size_15_19', cat='name', order_cat=TRUE, zero_line=TRUE, dt=d, country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/stage3_v3.pdf'), width = 12, height = 6)
mod[[4]]
dev.off()

## STAGE 2 TABLES
#d <- readRDS('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/model_data.rds')
d[gbd_region=='Oceania', gbd_super_region := 'Oceania']
d[gbd_super_region == 'Southeast Asia, East Asia, and Oceania', gbd_super_region := 'East Asia']
d[gbd_super_region %in% c('East Asia','South Asia'), gbd_super_region := 'Asia']
#d <- d[name %in% model_countries[, name], ]
d <- d[log_out_rate >= -5]
#d <- d[year >= 1990, ]
all_ivs <- c(paste0('lag0_', c('r_size_20_24','r_size_25_29','r_size_10_19','r_size_15_24','r_size_15_19','r_size_15_29')),
             paste0('lag5_', c('r_size_20_24','r_size_25_29','r_size_10_19','r_size_15_24','r_size_15_19','r_size_15_29')))
all_ivs <- 'lag0_r_size_15_24'
dv <- 'log_out_rate'
d[, net_out_migration := net_migration * -1]
d <- d[!is.na(lag5_r_size_15_19)]
d <- d[!is.na(r_size_15_19)]
d <- d[!is.na(prop_15_29)]
d <- d[!is.na(ratio_15_19_20_24)]
d <- d[!is.na(gbd_super_region)]
d[, ratio_15_19_20_24 := ratio_15_19_20_24 * 10]
## outliers
d[, country_year := paste0(name,'_',year)]
outliers <- c('Thailand','Timor-Leste','India','Myanmar','China')
outliers_cy <- c("Bahrain_2005","Lebanon_1990","Qatar_2005","Sudan_1990")
outliers <- c('Timor-Leste','Bhutan')
outliers_cy <- c('Thailand_1995','Nepal_2005','Maldives_1990',
                 'Jordan_1990','Lebanon_1990','Malaysia_2005',
                 'Nepal_2005','Maldives_1990')
outliers <- ''
outliers_cy <- ''
other_fes <- paste0(' + epr*',iv,' + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks')
#other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks')
d <- d[gbd_super_region!='Oceania',]

library(rsq)
library(relaimpo)
library(dplyr)
#all_mig <- copy(d)
#d <- copy(all_mig)
d <- merge(d, polity, by=c('name','year'), all.x=TRUE)
outliers <- ''
outliers_cy <- ''
## Demean everything
for(v in c('epr','r_ldi_pc','ldi_pc_gap','gbd_mx_shocks','edu','urbanicity',
           c(paste0('lag0_', c('r_size_20_24','r_size_25_29','r_size_10_19','r_size_15_24','r_size_15_19','r_size_15_29')),
           paste0('lag5_', c('r_size_20_24','r_size_25_29','r_size_10_19','r_size_15_24','r_size_15_19','r_size_15_29'))),
           "lag0_r_gbd_size_15_19","lag5_r_gbd_size_15_19",
           'log_out_rate','out_rate','net_out_migration','r_out_rate')) {
  for(c in unique(d[, name])) {
    c_mean <- mean(d[name==c, get(v)])
    d[name==c, (paste0('dmean_',v)) := get(v) - c_mean]
    d[name==c, (paste0('cmean_',v)) := c_mean]
  }
}

## Most positive correlations:
## log_out_rate:lag5_r_size_20_24
## log_out_rate:lag5_r_size_10_19
## net_out_migration:lag5_r_size_10_19
## log_out_rate:lag5_r_size_15_24
## net_out_migration:lag5_r_size_15_24
## log_out_rate:lag5_r_size_15_29
dv <- 'log_out_rate'
iv <- 'lag0_r_size_15_24'
other_fes <- paste0(' + epr*',iv,' + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks')
#other_fes <- paste0(' + epr*',iv,' + r_ldi_pc + ldi_pc_gap + edu + gbd_mx_shocks')
#other_fes <- paste0(' + dmean_r_ldi_pc + dmean_ldi_pc_gap + dmean_gbd_mx_shocks')
#other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + urbanicity + epr + edu + gbd_mx_shocks')
#other_fes <- ''
#f <- as.formula(paste0(paste0('dmean_',dv), ' ~ ', paste0('dmean_',iv,'*ldi_pc'), other_fes, ' + as.factor(name)'))
f <- as.formula(paste0(paste0('',dv), ' ~ ', paste0('',iv,''), other_fes, ' + as.factor(name)'))

d[,country_year := paste0(name,'_',year)]
model_data_ssa <- d[gbd_super_region=='Sub-Saharan Africa' & !(name %in% outliers) & !(country_year %in% outliers_cy),]
model_data_global <- d[!(name %in% outliers) & !(country_year %in% outliers_cy),]
mod_ssa <- lm(formula = f, data = model_data_ssa)
mod_global <- lm(formula = f, data = model_data_global)
coefs <- data.table(model='SSA',
                    name=names(mod_ssa$coefficients),
                    coef=mod_ssa$coefficients,
                    se=coef(summary(mod_ssa))[,2],
                    p=coef(summary(mod_ssa))[,4])
rsq(mod_ssa)
coefs[!grep('as.factor',name), ]
interplot(m = mod_ssa, var1 = iv, var2 = 'epr') + theme_minimal()

shapley_ssa <- calc.relimp(mod_ssa, type="lmg", rela=F)@lmg
shapley_global <- calc.relimp(mod_global, type="lmg", rela=F)@lmg
decomp_data <- data.table(predictor=names(shapley_ssa),
                          SSA=shapley_ssa,
                          Global=shapley_global)
decomp_data <- melt(decomp_data, id.vars = 'predictor', measure.vars = c('SSA','Global'), variable.name = 'model')
cov_map <- data.table(predictor=c('lag0_r_size_15_24','lag0_r_size_15_24:epr','epr','ldi_pc_gap','r_ldi_pc','gbd_mx_shocks','edu','as.factor(name)'),
                      cov_name=c('Growth rate (15-24)','Growth*EPR','EPR','LDI/pc gap','LDI/pc growth rate','Mortality shocks','Education','Country FEs'),
                      cov_order=1:8)
decomp_data <- merge(decomp_data, cov_map, by='predictor')
decomp_data[, cov_name := factor(cov_name, levels=cov_map[order(cov_order), cov_name])]
#pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/r_decomp.pdf'), width = 12, height = 8)
cols <- c('#a6cee3','#1f78b4','#fb9a99','#b2df8a','#33a02c','#6a3d9a','#ff7f00','#b15928')
ggplot() +
  geom_bar(data=decomp_data,
           aes(x=model,
               y=value,
               fill=cov_name),
           color='black',
           stat='identity') + theme_minimal() +
  coord_flip() + 
  scale_fill_manual(values = c(cols[1:length(unique(decomp_data[, predictor]))-1], '#ffff99'),
                    name = "Predictor") + 
  labs(title='Decomposition of R^2 by predictor', x='Model', y='Variance explained (log out-migration rate)')
#dev.off()

all_dv <- paste0('', c('log_out_rate','out_rate','net_out_migration'))
all_iv <- paste0('', c(paste0('lag0_', c('r_size_20_24','r_size_25_29','r_size_10_19','r_size_15_24','r_size_15_19','r_size_15_29')),
              paste0('lag5_', c('r_size_20_24','r_size_25_29','r_size_10_19','r_size_15_24','r_size_15_19','r_size_15_29')),
            "lag0_r_gbd_size_15_19","lag5_r_gbd_size_15_19"))
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/correlations.pdf'), width = 12, height = 8)
ssa_data <- d[gbd_super_region=='Asia' & year>=1990,]
global_data <- d[year>=1990,]
for(iv in all_iv) {
  for(dv in all_dv) {
    message(paste0(iv, ' : ', dv))
    other_fes <- ''
    f <- as.formula(paste0(paste0('',dv), ' ~ ', paste0('',iv,''), other_fes, ' + as.factor(name)'))
    mod_ssa <- lm(formula = f, data = ssa_data)
    mod_global <- lm(formula = f, data = global_data)
    gg <- ggplot() + 
      geom_point(data=ssa_data,
                 aes(x=get(iv),y=get(dv))) + 
      labs(x=iv,y=dv,title=paste0('Global coefficient: ', round(summary(mod_global)$coefficients[2,1], 2),
                                  ' (p=',round(summary(mod_global)$coefficients[2,4],2), ')\nSSA coefficient: ',
                                  round(summary(mod_ssa)$coefficients[2,1],2), ' (p=', 
                                  round(summary(mod_ssa)$coefficients[2,4],2),')\nSSA data plotted below.')) + 
      theme_minimal()
    print(gg)
  }
}
dev.off()


file <- 'edu'
for(iv in all_ivs) {
  
  message(paste0('Fitting ', iv, '...'))
  
  ## Save table of countries in each region.
  pull_region_countries <- function(r) {
    return(data.table(Region = r, Countries = paste(unique(d[gbd_super_region==r, name]), collapse=', ')))
  }
  region_country_map <- rbindlist(lapply(unique(d[, gbd_super_region]), pull_region_countries))
  saveRDS(region_country_map, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/region_country_map.rds')
  
  ## Run global model.
  model_data <- d[!(name %in% outliers) & !(country_year %in% outliers_cy),]
  global_mod <- glm(formula = f, data = model_data)
  global_coefs <- data.table(model='All countries',
                      name=names(global_mod$coefficients),
                      coef=global_mod$coefficients,
                      se=coef(summary(global_mod))[,2],
                      p=coef(summary(global_mod))[,4])

  ## Run region models.
  run_region_lm <- function(n) {
    model_data <- d[gbd_super_region==n & !(name %in% outliers) & !(country_year %in% outliers_cy),]
    mod <- glm(formula = f, data = model_data)
    coefs <- data.table(model=n,
                        name=names(mod$coefficients),
                        coef=mod$coefficients,
                        se=coef(summary(mod))[,2],
                        p=coef(summary(mod))[,4])
    return(coefs)
  }
  message('Fitting region LMs...')
  reg_models <- rbindlist(lapply(c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'),
                                 run_region_lm))
  reg_models <- rbind(reg_models, global_coefs, fill=TRUE)
  saveRDS(reg_models, paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/', file, '_stage2_models_', iv, '.RDS'))
  
}

## STAGE 2- look for outliers in growth vs. out-migration
d[, country_year := paste0(name,'_',year)]
r <- 'East Asia'
cols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
outliers <- c('Thailand','Timor-Leste','India','Myanmar','China')
outliers_cy <- c("Bahrain_2005","Lebanon_1990","Qatar_2005","Sudan_1990")
ggplot() +
  geom_line(data=d[gbd_super_region==r & !(name %in% outliers) & !(country_year %in% outliers_cy)],
            aes(x=lag0_r_size_15_24,
                y=log_out_rate,
                color=name),
            size=2) + 
  geom_point(data=d[gbd_super_region==r & !(name %in% outliers) & !(country_year %in% outliers_cy)],
             aes(x=lag0_r_size_15_24,
                 y=log_out_rate),
             size=3) + 
  scale_color_manual(values = cols) + 
  theme_minimal()

## STAGE 3 - random slope models
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/epr_out_curve.pdf'), width = 8, height = 6)
ggplot(data = d[!(gbd_super_region %in% c('High-income','Central Europe, Eastern Europe, and Central Asia','Oceania')), ],
       aes(x = epr,
           y = out_rate)) + 
  geom_point() + 
  geom_smooth() +
  ylim(c(0,200)) + 
  labs(x='Employment-population ratio',y='Out-migration rate (per 1000)') +
  ggtitle('Out-migration vs. employment-population ratio (absorpative capacity) for all country-years.') + 
  theme_minimal()
dev.off()

mod <- fit_random_slope(model_name='random', dv='log_out_rate', iv='lag5_r_size_15_19', cat='name', order_cat=TRUE, zero_line=TRUE, dt=d, country_fe=FALSE, guide_title = 'Out-migration\nper thousand', size_var = 'out_rate')
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/stage3_v3.pdf'), width = 12, height = 6)
mod[[1]]
mod[[4]]
dev.off()
