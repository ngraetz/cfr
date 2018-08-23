library(rsq)
library(relaimpo)
library(plyr)
library(data.table)
library(ggplot2)
library(olsrr)
library(interplot)

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

## STAGE 2 TABLES
d <- readRDS('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/model_data_2018-08-16.rds')
## Fix DRC
drc <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/drc_migration.csv')
setnames(drc,'out_rate','new_out_rate')
d <- merge(d, drc, by=c('name','year'), all.x=TRUE)
d[name=='Democratic Republic of the Congo', out_rate := new_out_rate]
d[name=='Democratic Republic of the Congo', log_out_rate := log(out_rate + 0.000001)]
d[, new_out_rate := NULL]

d <- d[year %in% c(1990,1995,2000,2005), ]
d[gbd_region=='Oceania', gbd_super_region := 'Oceania']
d[gbd_super_region == 'Southeast Asia, East Asia, and Oceania', gbd_super_region := 'East Asia']
d[gbd_super_region %in% c('East Asia','South Asia'), gbd_super_region := 'Asia']
d <- d[gbd_super_region!='Oceania',]
#d <- d[name %in% model_countries[, name], ]
#d <- d[log_out_rate <= -6, log_out_rate := log(0.01)] ## Smaller offset - now only half my smallest observation.
#d <- d[year >= 1990, ]
d[, net_out_migration := net_migration * -1]
d <- d[!is.na(lag5_r_size_15_19)]
d <- d[!is.na(r_size_15_19)]
d <- d[!is.na(prop_15_29)]
d <- d[!is.na(ratio_15_19_20_24)]
d <- d[!is.na(gbd_super_region)]
d[, ratio_15_19_20_24 := ratio_15_19_20_24 * 10]
d[, country_year := paste0(name,'_',year)]
#d[, urbanicity := urbanicity / 100]
#d[, epr := epr / 100]

## Try a couple new variables for absorptive capacity.
## Merge EPR 
ilo_epr <- fread('C:/Users/ngraetz/Downloads/ILOSTAT_epr.csv')
ilo_epr <- ilo_epr[sex=='SEX_T' & classif1.label=='Age: 15-24', ]
setnames(ilo_epr, 'ref_area', 'ihme_loc_id')
ilo_epr <- merge(ilo_epr, locs, by='ihme_loc_id')
setnames(ilo_epr, 'obs_value', 'epr')
setnames(ilo_epr, 'time', 'year')
ilo_epr <- ilo_epr[, c('name','year','epr')]
ilo_epr_1990 <- ilo_epr[year==1991, ]
ilo_epr_1990[, year := 1990]
ilo_epr <- rbind(ilo_epr, ilo_epr_1990)
setnames(ilo_epr, 'epr', 'epr_15_24')
d <- merge(d, ilo_epr, by=c('name','year'), all.x=TRUE)

## Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate)
ilo_unemp <- fread("C:/Users/ngraetz/Downloads/API_SL.UEM.1524.ZS_DS2_en_csv_v2_10034482/API_SL.UEM.1524.ZS_DS2_en_csv_v2_10034482.csv")
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

## Demean everything
for(v in c('epr','r_ldi_pc','ldi_pc_gap','gbd_mx_shocks','edu','urbanicity', 'polity2','epr_15_24','unemp_15_24','log_ldi_pc','log_arable_pc',
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

## Make EPR quantiles
# rev(seq(.1,1,.9/3))
for(q in c(1,.75,.5,.25)) {
  epr_q <- quantile(d[, epr], p=q)
  epr_15_24_q <- quantile(d[, epr_15_24], p=q)
  unemp_15_24_q <- quantile(d[, unemp_15_24], p=q)
  log_ldi_pc_q <- quantile(d[, log_ldi_pc], p=q)
  message(round(log_ldi_pc_q, 3))
  d[epr <= epr_q, epr_group := as.character(round(epr_q,0))]
  d[epr_15_24 <= epr_15_24_q, epr_15_24_group := as.character(round(epr_15_24_q))]
  d[unemp_15_24 <= unemp_15_24_q, unemp_15_24_group := as.character(round(unemp_15_24_q))]
  d[log_ldi_pc <= log_ldi_pc_q, log_ldi_pc_group := as.character(round(log_ldi_pc_q))]
}

## Read Stage-1 country-years
write.csv(data.table(country_year=unique(d[, country_year])), 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/stage2_countries.csv', row.names=FALSE)
stage1 <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/stage1_transition_countries.csv')
#d[country_year %in% stage1[, name], stage1 := 1]
d[name %in% stage1[, name], stage1 := 1]
stage1_4 <- fread('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/stage1_transition_countries_4.csv')
d[name %in% stage1_4[, name], stage1_4 := 1]


## Most positive correlations:
## log_out_rate:lag5_r_size_20_24
## log_out_rate:lag5_r_size_10_19
## net_out_migration:lag5_r_size_10_19
## log_out_rate:lag5_r_size_15_24
## net_out_migration:lag5_r_size_15_24
## log_out_rate:lag5_r_size_15_29
all_regions <- c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean')
outliers_cy <- c('Yemen_1990','Guinea_1990')
outliers <- c('Burundi','Afghanistan')

#for(file in c('noedu','edu')) {
file <- 'noedu'
dv <- 'log_out_rate'
iv <- 'lag5_r_size_15_24'
#other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + urbanicity + gbd_mx_shocks + epr')
#other_fes <- paste0(' + ', iv, '*as.factor(epr_group)')
#other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks + polity2 + urbanicity + epr')
other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks + epr_15_24 + urbanicity + polity2')
other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks')

if(file=='edu') other_fes <- paste0(other_fes, ' + edu')
f <- as.formula(paste0(paste0('',dv), ' ~ ', paste0('',iv,''), other_fes, ' + as.factor(name)'))
outliers_cy <- c('Yemen_1990','Guinea_1990')
outliers <- c('Burundi','Afghanistan')

## Run all models
## Run global model and Stage1 model.
#model_data_global <- d[gbd_super_region=='Sub-Saharan Africa' & !(name %in% outliers) & !(country_year %in% outliers_cy),]
model_data_global <- d[gbd_super_region %in% all_regions & !(name %in% outliers) & !(country_year %in% outliers_cy),]
mod_global <- lm(formula = f, data = model_data_global)
global_coefs <- data.table(model='All countries',
                    name=names(mod_global$coefficients),
                    coef=mod_global$coefficients,
                    se=coef(summary(mod_global))[,2],
                    p=coef(summary(mod_global))[,4],
                    r=summary(mod_global)$r.squared,
                    r_adj=summary(mod_global)$adj.r.squared)
global_coefs[!(grep('as.factor', name)), ]
global_coefs <- rbind(global_coefs, data.table(model='All countries', name=c('R^2','Adj. R^2','N'), coef=c(summary(mod_global)$r.squared, summary(mod_global)$adj.r.squared, dim(model_data_global)[1])), fill=TRUE)
## Run global model and Stage1 model.
model_data_stage1 <- d[stage1==1 & !(name %in% outliers) & !(country_year %in% outliers_cy),]
mod_stage1 <- lm(formula = f, data = model_data_stage1)
stage1_coefs <- data.table(model='Stage1',
                    name=names(mod_stage1$coefficients),
                    coef=mod_stage1$coefficients,
                    se=coef(summary(mod_stage1))[,2],
                    p=coef(summary(mod_stage1))[,4],
                    r=summary(mod_stage1)$r.squared,
                    r_adj=summary(mod_stage1)$adj.r.squared)
stage1_coefs <- rbind(stage1_coefs, data.table(model='Stage1', name=c('R^2','Adj. R^2','N'), coef=c(summary(mod_stage1)$r.squared, summary(mod_stage1)$adj.r.squared, dim(model_data_stage1)[1])), fill=TRUE)
## Run global model and Stage1 model. (4/4)
model_data_stage1_v2 <- d[stage1_4==1 & !(name %in% outliers) & !(country_year %in% outliers_cy),]
mod_stage1 <- lm(formula = f, data = model_data_stage1_v2)
stage1_v2_coefs <- data.table(model='Stage1_v2',
                           name=names(mod_stage1$coefficients),
                           coef=mod_stage1$coefficients,
                           se=coef(summary(mod_stage1))[,2],
                           p=coef(summary(mod_stage1))[,4],
                           r=summary(mod_stage1)$r.squared,
                           r_adj=summary(mod_stage1)$adj.r.squared)
stage1_v2_coefs <- rbind(stage1_v2_coefs, data.table(model='Stage1_v2', name=c('R^2','Adj. R^2','N'), coef=c(summary(mod_stage1)$r.squared, summary(mod_stage1)$adj.r.squared, dim(model_data_stage1)[1])), fill=TRUE)
## Run region models.
run_region_lm <- function(n) {
  model_data <- d[gbd_super_region==n & !(name %in% outliers) & !(country_year %in% outliers_cy),]
  mod <- lm(formula = f, data = model_data)
  coefs <- data.table(model=n,
                      name=names(mod$coefficients),
                      coef=mod$coefficients,
                      se=coef(summary(mod))[,2],
                      p=coef(summary(mod))[,4])
  coefs <- rbind(coefs, data.table(model=n, name=c('R^2','Adj. R^2','N'), coef=c(summary(mod)$r.squared, summary(mod)$adj.r.squared, dim(model_data)[1])), fill=TRUE)
  return(coefs)
}
message('Fitting region LMs...')
reg_models <- rbindlist(lapply(c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'),
                               run_region_lm))
reg_models <- rbind(reg_models, global_coefs, stage1_coefs, stage1_v2_coefs, fill=TRUE)
## Make correlation matrices.
clean_names <- function(x, matrix) {
  rownames(matrix) <- gsub(x,'',rownames(matrix))
  colnames(matrix) <- gsub(x,'',colnames(matrix))
  return(matrix)
}
global_cor_matrix <- round(cor(model_data_global[, c('dmean_log_out_rate','dmean_lag5_r_size_15_24','dmean_r_ldi_pc','dmean_ldi_pc_gap','dmean_gbd_mx_shocks','dmean_polity2','dmean_urbanicity','dmean_epr','dmean_edu')], use='complete.obs'),2)
global_cor_matrix <- clean_names(x='dmean_', global_cor_matrix)
global_cor_matrix <- clean_names(x='lag5_', global_cor_matrix)
stage1_cor_matrix <- round(cor(model_data_stage1[, c('dmean_log_out_rate','dmean_lag5_r_size_15_24','dmean_r_ldi_pc','dmean_ldi_pc_gap','dmean_gbd_mx_shocks','dmean_polity2','dmean_urbanicity','dmean_epr','dmean_edu')], use='complete.obs'),2)
stage1_cor_matrix <- clean_names(x='dmean_', stage1_cor_matrix)
stage1_cor_matrix <- clean_names(x='lag5_', stage1_cor_matrix)
africa_cor_matrix <- round(cor(d[gbd_super_region == 'Sub-Saharan Africa' & !(name %in% outliers) & !(country_year %in% outliers_cy),][, c('dmean_log_out_rate','dmean_lag5_r_size_15_24','dmean_r_ldi_pc','dmean_ldi_pc_gap','dmean_gbd_mx_shocks','dmean_polity2','dmean_urbanicity','dmean_epr','dmean_edu')], use='complete.obs'),2)
africa_cor_matrix <- clean_names(x='dmean_', africa_cor_matrix)
africa_cor_matrix <- clean_names(x='lag5_', africa_cor_matrix)
saveRDS(list(global_cor_matrix,stage1_cor_matrix,africa_cor_matrix), 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/cor_matrices.RDS')
saveRDS(reg_models, paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/', file, '_stage2_models_', iv, '_', Sys.Date(), '.RDS'))
#}

## Check for outliers.
library(influence.ME)
library(lme4)
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/outliers_dfbetas.pdf'), width = 8, height = 6)
for(n in c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean')) {
  message(paste0('Outlier plots: ', n))
  #model_data <- d[gbd_super_region==n & !(name %in% outliers) & !(country_year %in% outliers_cy),]
  # model_data <- d[stage1==1 & !(name %in% outliers) & !(country_year %in% outliers_cy),]
  # f <- as.formula(paste0(paste0('',dv), ' ~ ', paste0('',iv,''), other_fes, ' + (1|name)'))
  # mod <- lmer(formula = f, data = model_data)
  
  message(int_iv)
  #d[, abs_cap := as.numeric(get(int_iv))]
  #file <- c('abs_cap_int')
  all_regions <- c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean')
  #if(file=='abs_cap_int') epr_name <- ':abs_cap'
  #if(file=='abs_cap') epr_name <- 'abs_cap'
  use_edu <- 'noedu'
  dv <- 'log_out_rate'
  other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks + polity2 + urbanicity')
  if(use_edu=='edu') other_fes <- paste0(other_fes, ' + edu')
  other_fes <- paste0(other_fes, ' + ', int_iv, '*', iv)
  f <- as.formula(paste0(paste0('',dv), ' ~ ', paste0('',iv,''), other_fes, ' + as.factor(name)'))
  outliers_cy <- c('Yemen_1990','Guinea_1990')
  outliers <- c('Burundi','Afghanistan')

  ## Calculate leverage
  dt <- d[gbd_super_region %in% all_regions & !(name %in% outliers) & !(country_year %in% outliers_cy),]
  mod <- lm(formula = f, data = dt)
  
  inf <- influence.ME::influence(mod, obs=TRUE)
  dt[, cooks := cooks.distance(inf)]
  dfbetas <- data.table(dfbetas(inf))
  dfbetas[, country_year := dt[, country_year]]
  dfbetas <- melt(dfbetas, id.vars = 'country_year', measure.vars = names(dfbetas)[names(dfbetas)!='country_year'])
  dfbetas_outliers <- dfbetas[value <= -(2/sqrt(dim(dfbetas)[1])) | value >= 2/sqrt(dim(dfbetas)[1]), ]
  gg <- ggplot(data=dfbetas_outliers) +
    geom_point(aes(x=country_year,
                   y=value)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_hline(yintercept = c(-(2/sqrt(dim(dfbetas)[1])),2/sqrt(dim(dfbetas)[1])), color='red') +
    ggtitle(n) +
    facet_wrap(~variable)
  print(gg)
}
dev.off()

## Correlation matrices
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/correlations_covs.pdf'), width = 12, height = 8)
## Make full dataset to facet by Global, Stage1, Regions
model_data_stage1[, gbd_super_region := 'Stage1']
model_data_global[, gbd_super_region := 'Global']
full_data <- rbind(d[gbd_super_region %in% all_regions & !(name %in% outliers) & !(country_year %in% outliers_cy),], model_data_stage1, model_data_global)
all_vars <- c('lag5_r_size_15_24','r_ldi_pc','ldi_pc_gap','gbd_mx_shocks','polity2','urbanicity','epr','edu')
for(iv in all_vars) {
message(iv)
other_fes <- ''
f <- as.formula(paste0(paste0('',dv), ' ~ ', paste0('',iv,''), other_fes, ' + as.factor(name)'))
mod_ssa <- lm(formula = f, data = model_data_stage1)
mod_global <- lm(formula = f, data = model_data_global)
gg <- ggplot() + 
  geom_point(data=full_data,
             aes(x=get(paste0('dmean_',iv)),y=get(paste0('dmean_',dv)))) + 
  # geom_text(data=full_data,
  #           aes(x=get(paste0('dmean_',iv)),y=get(paste0('dmean_',dv)),label=country_year)) +
  labs(x=iv,y=dv,title=paste0(iv,'\nGlobal coefficient: ', round(summary(mod_global)$coefficients[2,1], 2),
                              ' (p=',round(summary(mod_global)$coefficients[2,4],2), ')\nStage1 coefficient: ',
                              round(summary(mod_ssa)$coefficients[2,1],2), ' (p=', 
                              round(summary(mod_ssa)$coefficients[2,4],2), ')')) + 
  facet_wrap(~gbd_super_region) + 
  theme_minimal()
print(gg)
}
dev.off()


## Stage 3: try EPR quantiles vs. out-rate, and also EPR*growth vs. out-rate.
library(interplot)
iv <- 'lag5_r_size_15_24'
#pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/interaction_plots_', iv, '.pdf'), width = 12, height = 8)
#for(int_iv in c('epr','epr_15_24','unemp_15_24','log_ldi_pc')) {
int_iv <- 'log_ldi_pc'
message(int_iv)
d[, abs_cap := as.numeric(get(int_iv))]
#file <- c('abs_cap_int')
all_regions <- c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean')
#if(file=='abs_cap_int') epr_name <- ':abs_cap'
#if(file=='abs_cap') epr_name <- 'abs_cap'
  use_edu <- 'noedu'
  dv <- 'log_out_rate'
  #dv <- 'net_out_migration'
  #other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + urbanicity + gbd_mx_shocks + epr')
  #other_fes <- paste0(' + ', iv, '*as.factor(epr_group)')
  #other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks + polity2 + urbanicity')
  #other_fes <- paste0(' + epr + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks + polity2 + urbanicity')
  #other_fes <- paste0(' + dmean_epr + dmean_r_ldi_pc + dmean_ldi_pc_gap + dmean_gbd_mx_shocks + dmean_polity2 + dmean_urbanicity')
  #other_fes <- paste0(' + dmean_r_ldi_pc + dmean_ldi_pc_gap + dmean_gbd_mx_shocks + dmean_log_arable_pc')
  other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks + epr_15_24')
  if(use_edu=='edu') other_fes <- paste0(other_fes, ' + edu')
  other_fes <- paste0(other_fes, ' + ', int_iv, '*', iv)
  f <- as.formula(paste0(paste0('',dv), ' ~ ', paste0('',iv,''), other_fes, ' + as.factor(name)'))
  #f <- as.formula(paste0(paste0('',dv), ' ~ ', paste0('',iv,''), other_fes))
  outliers_cy <- c('Yemen_1990','Guinea_1990')
  #,'Bahrain_2005','Kenya_2005'
  outliers <- c('Burundi','Afghanistan')
  
  ## Run all models
  ## Run global model and Stage1 model.
  #model_data_global <- d[gbd_super_region=='Sub-Saharan Africa' & !(name %in% outliers) & !(country_year %in% outliers_cy),]
  #model_data_global <- d[gbd_super_region %in% all_regions & !(name %in% outliers) & !(country_year %in% outliers_cy),]
  model_data_global <- copy(d)
  model_data_global[, region_f := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean','High-income','Central Europe, Eastern Europe, and Central Asia'))]
  #model_data_global <- model_data_global[gbd_super_region %in% all_regions & !(name %in% outliers) & !(country_year %in% outliers_cy),]
  model_data_global <- model_data_global[gbd_super_region %in% all_regions & !(name %in% outliers) & !(country_year %in% outliers_cy),]
  model_data_global[, (paste0('original_', iv)) := get(iv)]
  model_data_global[, (iv) := get(iv) - mean(get(iv), na.rm=TRUE)]
  model_data_global[, (int_iv) := get(int_iv) - mean(get(int_iv), na.rm=TRUE)]
  mod_global <- lm(formula = f, data = model_data_global)
  #ols_plot_dfbetas(mod_global)
  global_coefs <- data.table(model='All countries',
                             name=names(mod_global$coefficients),
                             coef=mod_global$coefficients,
                             se=coef(summary(mod_global))[,2],
                             p=coef(summary(mod_global))[,4],
                             r=summary(mod_global)$r.squared,
                             r_adj=summary(mod_global)$adj.r.squared)
  global_coefs[!(grep('as.factor', name)), ]
  global_coefs <- rbind(global_coefs, data.table(model='All countries', name=c('R^2','Adj. R^2','N'), coef=c(summary(mod_global)$r.squared, summary(mod_global)$adj.r.squared, dim(model_data_global)[1])), fill=TRUE)

  #pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/stage3_ldi_interaction_with_high_income',iv,'.pdf'), width = 20, height = 10)
  pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_2_', Sys.Date(), '.pdf'), width = 20, height = 10)
  for(q in c(7,4,2,0)) {
    model_data_global[original_lag5_r_size_15_24 <= q, growth_group := as.character(round(q,0))]
  }
  global_interplot <- interplot(m = mod_global, var1 = iv, var2 = int_iv) +
    geom_hline(yintercept = 0, color='red') +
    geom_vline(xintercept = 0, color='red') +
    ggtitle('Figure 2. Conditional coefficient for growth on out-migration based on level of lag-distributed income per capita.') +
    ylab(paste0('Conditional coefficient of growth on log(out-migration rate)')) + xlab(paste0('Level of mean-centered log(LDI/pc)')) + theme_minimal()
  global_interplot +
    geom_jitter(data=model_data_global,
               aes(x=log_ldi_pc,
                   y=log_ldi_pc * mod_global$coefficients[paste0(iv,':',int_iv)] + mod_global$coefficients[iv],
                   alpha = growth_group,
                   fill = region_f),
               size=8,
               height = 0.1,
               width = 0.1,
               shape = 21) +
    #scale_size(name='Growth rate in 15-24 (lag-5)', breaks=c(0,2,4,6,8), range=c(1,10)) +
    scale_alpha_manual(values=c(.2,.5,.8,1),name='Growth rate in 15-24 (lag-5)',labels=c('< 0','0-2','2-4','4+')) + 
    scale_fill_manual(name='GBD Super Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) +
    guides(fill = guide_legend(override.aes = list(size = 10)))
  dev.off()
  coef_data <- data.table(global_interplot$data)
  setnames(coef_data, 'fake', 'log_ldi_pc')
  write.csv(coef_data, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_2_coef_data.csv', row.names=FALSE)
  scatter_data <- copy(model_data_global)
  scatter_data[, predicted_coef := log_ldi_pc * mod_global$coefficients[paste0(iv,':',int_iv)] + mod_global$coefficients[iv]]
  scatter_data <- scatter_data[, c('name','year','region_f','original_lag5_r_size_15_24','log_ldi_pc','predicted_coef')]
  write.csv(scatter_data, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_2_scatter_data.csv', row.names=FALSE)
  
  #global_coefs <- rbind(global_coefs, data.table(model='All countries', name=paste0(epr_name,'_ref'), coef=0), fill=TRUE)
  #interplot(m = mod_global, var1 = iv, var2 = 'abs_cap') + theme_minimal()
  ## Run global model and Stage1 model.
  model_data_stage1 <- d[stage1==1 & !(name %in% outliers) & !(country_year %in% outliers_cy),]
  model_data_stage1[, (iv) := get(iv) - mean(get(iv), na.rm=TRUE)]
  model_data_stage1[, (int_iv) := get(int_iv) - mean(get(int_iv), na.rm=TRUE)]
  mod_stage1 <- lm(formula = f, data = model_data_stage1)
  stage1_coefs <- data.table(model='Stage1',
                             name=names(mod_stage1$coefficients),
                             coef=mod_stage1$coefficients,
                             se=coef(summary(mod_stage1))[,2],
                             p=coef(summary(mod_stage1))[,4],
                             r=summary(mod_stage1)$r.squared,
                             r_adj=summary(mod_stage1)$adj.r.squared)
  stage1_coefs[!(grep('as.factor', name)), ]
  stage1_coefs <- rbind(stage1_coefs, data.table(model='Stage1', name=c('R^2','Adj. R^2','N'), coef=c(summary(mod_stage1)$r.squared, summary(mod_stage1)$adj.r.squared, dim(model_data_stage1)[1])), fill=TRUE)
  #stage1_coefs <- rbind(stage1_coefs, data.table(model='Stage1', name=paste0(epr_name,'_ref'), coef=0), fill=TRUE)
  #interplot(m = mod_stage1, var1 = iv, var2 = 'abs_cap') + theme_minimal()
  ## Run region models.
  run_region_lm <- function(n) {
    message(n)
    model_data <- d[gbd_super_region==n & !(name %in% outliers) & !(country_year %in% outliers_cy),]
    model_data[, (iv) := get(iv) - mean(get(iv), na.rm=TRUE)]
    model_data[, (int_iv) := get(int_iv) - mean(get(int_iv), na.rm=TRUE)]
    mod <- lm(formula = f, data = model_data)
    coefs <- data.table(model=n,
                        name=names(mod$coefficients)[!grepl('as.factor',names(mod$coefficients))],
                        coef=mod$coefficients[!grepl('as.factor',names(mod$coefficients))],
                        se=coef(summary(mod))[,2][!grepl('as.factor',names(coef(summary(mod))[,2]))],
                        p=coef(summary(mod))[,4][!grepl('as.factor',names(coef(summary(mod))[,4]))])
    coefs <- rbind(coefs, data.table(model=n, name=c('R^2','Adj. R^2','N'), coef=c(summary(mod)$r.squared, summary(mod)$adj.r.squared, dim(model_data)[1])), fill=TRUE)
    #coefs <- rbind(coefs, data.table(model=n, name=paste0(epr_name,'_ref'), coef=0), fill=TRUE)
    #interplot(m = mod, var1 = iv, var2 = 'abs_cap') + theme_minimal()
    return(coefs)
  }
  message('Fitting region LMs...')
  reg_models <- rbindlist(lapply(c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'),
                                 run_region_lm))
  reg_models <- rbind(reg_models, global_coefs, stage1_coefs, fill=TRUE)
  saveRDS(reg_models, paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/stage3_models_', iv, '_', Sys.Date(), '.RDS'))
  # pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/', file, '.pdf'), width = 12, height = 8)
  # plot_data <- reg_models[grep(epr_name,name), ]
  # plot_data[, name := gsub('lag5_r_size_15_24','',name)]
  # plot_data[, name := gsub(':','',name)]
  # gg <- ggplot(plot_data) +
  #   geom_point(aes(x=name,
  #                  y=coef),
  #              size=5) +
  #   geom_errorbar(aes(x=name,
  #                     ymin=coef-(se*1.96),
  #                     ymax=coef+(se*1.96))) +
  #   geom_hline(yintercept = 0, color='red', size=1) +
  #   labs(y='Coefficient (log out-rate)', x='EPR quantile', title='Non-linear relationship between out-migration and EPR.') +
  #   facet_wrap(~model) +
  #   theme_minimal()
  # print(gg)
  # dev.off()

  # run_region_interplot <- function(n) {
  # model_data <- d[gbd_super_region==n & !(name %in% outliers) & !(country_year %in% outliers_cy),]
  # model_data[, (iv) := get(iv) - mean(get(iv))]
  # model_data[, (int_iv) := get(int_iv) - mean(get(int_iv))]
  # mod <- lm(formula = f, data = model_data)
  # int <- interplot(m = mod, var1 = iv, var2 = int_iv) + 
  #   geom_hline(yintercept = 0, color='red') + 
  #   geom_vline(xintercept = 0, color='red') + 
  #   ggtitle(paste0(n, ifelse(summary(mod)$coefficients[paste0(iv,':',int_iv),4] < 0.05, ' - SIGNIFICANT', ''))) + 
  #   ylab(paste0('Coefficient on ', iv)) + xlab(paste0('Level of mean-centered ', int_iv)) + theme_minimal()
  # return(int)
  # }
  # global_interplot <- interplot(m = mod_global, var1 = iv, var2 = int_iv, hist=TRUE) + 
  #   geom_hline(yintercept = 0, color='red') + 
  #   geom_vline(xintercept = 0, color='red') + 
  #   ggtitle(paste0('All countries', ifelse(summary(mod_global)$coefficients[paste0(iv,':',int_iv),4] < 0.05, ' - SIGNIFICANT', ''))) + 
  #   ylab(paste0('Coefficient on ', iv)) + xlab(paste0('Level of mean-centered ', int_iv)) + theme_minimal()
  # stage1_interplot <- interplot(m = mod_stage1, var1 = iv, var2 = int_iv) + 
  #   geom_hline(yintercept = 0, color='red') + 
  #   geom_vline(xintercept = 0, color='red') + 
  #   ggtitle(paste0('Stage 1', ifelse(summary(mod_stage1)$coefficients[paste0(iv,':',int_iv),4] < 0.05, ' - SIGNIFICANT', ''))) + 
  #   ylab(paste0('Coefficient on ', iv)) + xlab(paste0('Level of mean-centered ', int_iv)) + theme_minimal()
  # all_interplots <- lapply(c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'), run_region_interplot)
  # all_interplots[[5]] <- global_interplot
  # all_interplots[[6]] <- stage1_interplot
  # int_plots <- marrangeGrob(all_interplots, nrow=2, ncol=3, top=int_iv)
  # print(int_plots)
  saveRDS(reg_models, paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/', iv, 'interaction_stage2_models_', int_iv, '_',Sys.Date(),'.RDS'))
  
#}
# dev.off()

library(rmarkdown)
render("C:/Users/ngraetz/Documents/repos/cfr_migration/all_tables.Rmd", output_file=paste0("C:/Users/ngraetz/Documents/repos/cfr_migration/all_tables_", Sys.Date(), ".pdf"))


## Try to make effect size plots.
int_iv <- 'log_ldi_pc'
iv <- 'lag5_r_size_15_24'
message(int_iv)
d[, abs_cap := as.numeric(get(int_iv))]
all_regions <- c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean')
use_edu <- 'noedu'
dv <- 'log_out_rate'
#other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks + polity2 + urbanicity')
#other_fes <- paste0(' + dmean_epr + dmean_r_ldi_pc + dmean_ldi_pc_gap + dmean_gbd_mx_shocks + dmean_polity2 + dmean_urbanicity + dmean_log_arable_pc')
#other_fes <- paste0(' + dmean_r_ldi_pc + dmean_ldi_pc_gap + dmean_gbd_mx_shocks + dmean_log_arable_pc')
#other_fes <- paste0(' + dmean_r_ldi_pc + dmean_ldi_pc_gap + dmean_gbd_mx_shocks + dmean_epr_15_24')
other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks + epr_15_24 + polity2 + urbanicity')
#other_fes <- ' + log_ldi_pc'
if(use_edu=='edu') other_fes <- paste0(other_fes, ' + edu')
other_fes <- paste0(other_fes, ' + ', int_iv, '*', iv)
f <- as.formula(paste0(paste0('',dv), ' ~ ', paste0('',iv,''), other_fes, ' + as.factor(name)'))
outliers_cy <- c('Yemen_1990','Guinea_1990')
outliers <- c('Burundi','Afghanistan')

clean_names <- function(x, matrix) {
  rownames(matrix) <- gsub(x,'',rownames(matrix))
  colnames(matrix) <- gsub(x,'',colnames(matrix))
  return(matrix)
}

## Run all models
model_data_global <- copy(d[year %in% c(1990,1995,2000,2005)])
model_data_global[, name := gsub('\\(','',name)]
model_data_global[, name := gsub('\\)','',name)]
model_data_global[, region_f := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean','High-income','Central Europe, Eastern Europe, and Central Asia'))]
model_data_global <- model_data_global[gbd_super_region %in% all_regions & !(name %in% outliers) & !(country_year %in% outliers_cy),]
model_data_global[, (paste0('original_', iv)) := get(iv)]
model_data_global[, (iv) := get(iv) - mean(get(iv), na.rm=TRUE)]
model_data_global[, (int_iv) := get(int_iv) - mean(get(int_iv), na.rm=TRUE)]
global_cor_matrix <- round(cor(model_data_global[, c('dmean_log_out_rate','dmean_lag5_r_size_15_24','dmean_r_ldi_pc','dmean_ldi_pc_gap','dmean_gbd_mx_shocks','dmean_polity2','dmean_urbanicity','dmean_epr','dmean_edu','dmean_log_arable_pc')], use='complete.obs'),2)
global_cor_matrix <- clean_names(x='dmean_', global_cor_matrix)
global_cor_matrix <- clean_names(x='lag5_', global_cor_matrix)
mod_global <- lm(formula = f, data = model_data_global)
global_coefs <- data.table(model='All countries',
                           name=names(mod_global$coefficients),
                           coef=mod_global$coefficients,
                           se=coef(summary(mod_global))[,2],
                           p=coef(summary(mod_global))[,4],
                           r=summary(mod_global)$r.squared,
                           r_adj=summary(mod_global)$adj.r.squared)
global_coefs[!(grep('as.factor', name)), ]
global_coefs <- rbind(global_coefs, data.table(model='All countries', name=c('R^2','Adj. R^2','N'), coef=c(summary(mod_global)$r.squared, summary(mod_global)$adj.r.squared, dim(model_data_global)[1])), fill=TRUE)
for(v in c(int_iv,paste0('dmean_',c('r_ldi_pc','ldi_pc_gap','gbd_mx_shocks','polity2','urbanicity','epr')))) {
  model_data_global[, (paste0('cont_',v)) := get(v) * global_coefs[name==v, coef]]
}
model_data_global[, cont_interaction := get(iv) * (global_coefs[name==iv, coef] + (global_coefs[name==paste0(iv,':',int_iv), coef] * log_ldi_pc))]
for(c in c('Algeria',gsub('as.factor\\(name\\)','',global_coefs[grep('as.factor',name), name]))) {
  if(c!='Algeria') { 
  model_data_global[name==c, cont_country := global_coefs[name==paste0('as.factor(name)', c), coef]]
  }
  if(c=='Algeria') model_data_global[name==c, cont_country := 0]
}
model_data_global[, cont_intercept := global_coefs[name=='(Intercept)', coef]]
model_data_global[, pred_out := apply(.SD, 1, sum, na.rm=T), .SDcols=grep("^cont_", names(model_data_global))]

for(c in unique(model_data_global[, name])) {
  if(!(c %in% substr(global_coefs$name, 16, 50))) {
    message(paste0(c, ' is missing these variables...'))
    for(v in c(int_iv,'r_ldi_pc','ldi_pc_gap','gbd_mx_shocks','polity2','urbanicity'))
  }
}
model_data_global[108, c('name','log_out_rate','pred_out',"cont_log_ldi_pc","cont_dmean_r_ldi_pc","cont_dmean_ldi_pc_gap","cont_dmean_gbd_mx_shocks","cont_dmean_polity2","cont_dmean_urbanicity","cont_interaction","cont_country","cont_intercept")]
model_data_global[lag5_r_size_15_24 >= 2, c('log_out_rate','pred_out',"lag5_r_size_15_24","log_ldi_pc","cont_interaction")]

## Simulate uncertainty
library(arm)
sims <- sim(mod_global, 1000)
sims <- as.data.table(sims@coef)
sims[, draw := 1:1000]
sims[, index := 1]
draws <- dcast(sims, index ~ draw, value.var=c('lag5_r_size_15_24','lag5_r_size_15_24:log_ldi_pc'))

plot_data <- copy(model_data_global)
for(draw in 1:1000) {
  plot_data[, (paste0('cont_interaction_',draw)) := get(iv) * (draws[, get(paste0(iv,'_',draw))] + (draws[, get(paste0(iv,':',int_iv,'_',draw))] * log_ldi_pc))]
}
plot_data[, cont_interaction_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^cont_interaction_", names(plot_data))]
plot_data[, cont_interaction_upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^cont_interaction_", names(plot_data))]
plot_data[, cont_interaction_lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^cont_interaction_", names(plot_data))]
#plot_data[, country_f := factor(name, levels=plot_data$name[order(plot_data[, cont_interaction])])]
plot_data[, country_year_f := factor(country_year, levels=plot_data$country_year[order(plot_data[, cont_interaction_mean])])]
plot_data[, region_f := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_3_', Sys.Date(), '.pdf'), width = 12, height = 8)
ggplot() +
  geom_bar(data=plot_data[cont_interaction_lower >= 0 & cont_interaction_mean >= 0.75,],
           aes(x=country_year_f,
                y=cont_interaction,
                fill=region_f),
            color='black',
            stat='identity') + 
  geom_errorbar(data=plot_data[cont_interaction_lower >= 0 & cont_interaction_mean >= 0.75,],
                aes(x=country_year_f, ymin=cont_interaction_lower, ymax=cont_interaction_upper), width=.4) + 
  theme_minimal() + 
  scale_fill_manual(name='GBD Super Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) +
  labs(y='Total predicted log(out-migration) due to growth (15-24, lag-5)', x='', title='Total predicted increase in log(out-migration) associated with growth for\nall countries where the growth coefficient conditional on log(LDI/pc) is positive.') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot_data2 <- copy(plot_data[year==2005, ])
plot_data2[, country_f := factor(name, levels=plot_data2$name[order(plot_data2[, cont_interaction_mean])])]
#plot_data[, country_year_f := factor(country_year, levels=plot_data$country_year[order(plot_data[, cont_interaction])])]
plot_data2[, region_f := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]
ggplot() +
  geom_bar(data=plot_data2[cont_interaction_lower >= 0,],
           aes(x=country_f,
               y=cont_interaction_mean,
               fill=region_f),
           color='black',
           stat='identity') + 
  geom_errorbar(data=plot_data2[cont_interaction_lower >= 0,],
                aes(x=country_f, ymin=cont_interaction_lower, ymax=cont_interaction_upper), width=.4) + 
  theme_minimal() + 
  scale_fill_manual(name='GBD Super Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) +
  labs(y='Total predicted log(out-migration) due to growth (15-24, lag-5)', x='', title='2005-2010: Total predicted increase in log(out-migration) associated with growth for\nall countries where the growth coefficient conditional on log(LDI/pc) is positive.') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
dev.off()

write.csv(model_data_global[, c('name','year','out_rate','lag5_r_size_15_24',"log_ldi_pc","r_ldi_pc","ldi_pc_gap","gbd_mx_shocks","polity2","log_arable_pc")],
          'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/descriptives.csv', row.names = FALSE)

model_data_global[name %in% gsub('as.factor\\(name\\)','',global_coefs[grep('as.factor',name), name])
, full_pred := predict(mod_global, model_data_global[name %in% gsub('as.factor\\(name\\)','',global_coefs[grep('as.factor',name), name]),])]

model_data_global[name=='Ethiopia' & year==2005, cont_interaction + cont_log_ldi_pc + cont_r_ldi_pc + cont_ldi_pc_gap + cont_gbd_mx_shocks + cont_polity2 + cont_urbanicity + cont_intercept + cont_country]
model_data_global[name=='Ethiopia' & year==2005, c('name','log_out_rate','pred_out',"cont_log_ldi_pc","cont_dmean_r_ldi_pc","cont_dmean_ldi_pc_gap","cont_dmean_gbd_mx_shocks","cont_dmean_polity2","cont_dmean_urbanicity","cont_interaction","cont_country","cont_intercept")]

## Try Shapley decomposition for total number of migrants

model_data_global[name=='Ethiopia' & year==2005, cont_residual := mod_global$residuals['96']]
model_data_global[name=='Ethiopia' & year==2005, c("cont_dmean_epr","cont_residual","cont_log_ldi_pc","cont_dmean_r_ldi_pc","cont_dmean_ldi_pc_gap","cont_dmean_gbd_mx_shocks","cont_dmean_polity2","cont_dmean_urbanicity","cont_interaction","cont_country","cont_intercept")]
exp(model_data_global[name=='Ethiopia' & year==2005, apply(.SD, 1, sum, na.rm=T), .SDcols=c("cont_dmean_epr","cont_residual","cont_log_ldi_pc","cont_dmean_r_ldi_pc","cont_dmean_ldi_pc_gap","cont_dmean_gbd_mx_shocks","cont_dmean_polity2","cont_dmean_urbanicity","cont_interaction","cont_country","cont_intercept")])
exp(model_data_global[name=='Ethiopia' & year==2005, log_out_rate])
exp(model_data_global[name=='Ethiopia' & year==2005, pred_out])

compare <- exp(model_data_global[name=='Ethiopia' & year==2005, apply(.SD, 1, sum, na.rm=T), .SDcols=c('cont_intercept','cont_country')])
conts <- c()                  
for(v in c("cont_residual","cont_dmean_epr","cont_log_ldi_pc","cont_dmean_r_ldi_pc","cont_dmean_ldi_pc_gap","cont_dmean_gbd_mx_shocks","cont_dmean_polity2","cont_dmean_urbanicity","cont_interaction")) {
  
  conts <- c(conts, exp(model_data_global[name=='Ethiopia' & year==2005, apply(.SD, 1, sum, na.rm=T), .SDcols=c(v,'cont_intercept','cont_country')]) - compare)
}





library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
map <- readOGR("C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/shapefile//ne_50m_admin_0_countries.shp")
#map_simple <- gSimplify(map, tol = 0.5, topologyPreserve = TRUE)
map$plot_name <- as.character(map$name)
map$plot_name[map$plot_name=='Congo (Brazzaville)'] <- 'Congo'
map$plot_name[map$plot_name=='S. Sudan'] <- 'South Sudan'
map$plot_name[map$plot_name=='Eq. Guinea'] <- 'Equatorial Guinea'
map$plot_name[map$plot_name=='Guinea Bissau'] <- 'Guinea-Bissau'
map$plot_name[map$plot_name=='Dominican Rep.'] <- 'Dominican Republic'
map$plot_name[map$plot_name=='Central African Rep.'] <- 'Central African Republic'
map$plot_name[map$plot_name=='Tanzania'] <- "United Republic of Tanzania"
map$plot_name[map$plot_name=='Bolivia'] <- "Bolivia (Plurinational State of)"
map$plot_name[map$plot_name=='Iran'] <- "Iran (Islamic Republic of)"
map$plot_name[map$plot_name=='Ivory Coast'] <- "Cote d'Ivoire" 
map$plot_name[map$plot_name=='Congo (Kinshasa)'] <- "Democratic Republic of the Congo"
map$plot_name[map$plot_name=='Laos'] <- "Lao People's Democratic Republic" 
map$plot_name[map$plot_name=='Syria'] <- "Syrian Arab Republic"
map$plot_name[map$plot_name=='Venezuela'] <- "Venezuela (Bolivarian Republic of)"
map$plot_name[map$plot_name=='Vietnam'] <- "Viet Nam"
plot_data[!(name %in% map$plot_name), name]
plot_data[, plot_name := name]
map_sp <- merge(map, plot_data[year==2005, c('plot_name','cont_interaction')], by = 'plot_name')
map_data <- fortify(map_sp, region='plot_name')
map_data <- as.data.table(merge(map_data, map_sp@data, by.x = "id", by.y = "plot_name"))
map_data[cont_interaction <= 0, cont_interaction := 0]
map_data <- map_data[!(name %in% c('Antarctica','Greenland')),]
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_4_', Sys.Date(), '.pdf'), width = 20, height = 10)
this_gg <- ggplot() +
  geom_polygon(data = map_data,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cont_interaction),
               alpha = 0.8,
               color = 'black') +
  scale_fill_gradientn(name = "Growth rate *\nconditional coefficient", na.value = 'grey', colors=rev(brewer.pal(10,'Spectral')), limits=c(0,max(map_data$cont_interaction))) + 
  labs(x='',y='',title='Observed growth rate times fitted growth coefficient conditional on LDI/pc, 2005-2010.') + 
  theme_minimal()
this_gg
dev.off()

message(int_iv)
d[, abs_cap := as.numeric(get(int_iv))]
all_regions <- c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean')
use_edu <- 'noedu'
dv <- 'log_out_rate'
int_iv <- 'log_ldi_pc_group'
#other_fes <- paste0(' + r_ldi_pc + ldi_pc_gap + gbd_mx_shocks + polity2 + urbanicity')
other_fes <- paste0('dmean_epr + dmean_r_ldi_pc + dmean_ldi_pc_gap + dmean_gbd_mx_shocks + dmean_polity2 + dmean_urbanicity')
#other_fes <- ' + log_ldi_pc'
if(use_edu=='edu') other_fes <- paste0(other_fes, ' + edu')
other_fes <- paste0(other_fes, ' + ', int_iv, '*', iv)
f <- as.formula(paste0(paste0('',dv), ' ~ ', other_fes, ' + as.factor(name)'))
outliers_cy <- c('Yemen_1990','Guinea_1990')
outliers <- c('Burundi','Afghanistan')

## Run all models
model_data_global <- copy(d)
model_data_global[, region_f := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean','High-income','Central Europe, Eastern Europe, and Central Asia'))]
model_data_global <- model_data_global[gbd_super_region %in% all_regions & !(name %in% outliers) & !(country_year %in% outliers_cy),]
for(q in c(1,.75,.5,.25)) {
  epr_q <- quantile(model_data_global[, epr], p=q)
  epr_15_24_q <- quantile(model_data_global[, epr_15_24], p=q)
  unemp_15_24_q <- quantile(model_data_global[, unemp_15_24], p=q)
  log_ldi_pc_q <- quantile(model_data_global[, log_ldi_pc], p=q)
  message(round(log_ldi_pc_q, 3))
  model_data_global[epr <= epr_q, epr_group := as.character(round(epr_q,0))]
  model_data_global[epr_15_24 <= epr_15_24_q, epr_15_24_group := as.character(round(epr_15_24_q))]
  model_data_global[unemp_15_24 <= unemp_15_24_q, unemp_15_24_group := as.character(round(unemp_15_24_q))]
  model_data_global[log_ldi_pc <= log_ldi_pc_q, log_ldi_pc_group := as.character(round(log_ldi_pc_q,2))]
}
model_data_global[, (paste0('original_', iv)) := get(iv)]
model_data_global[, (iv) := get(iv) - mean(get(iv))]
#model_data_global[, (int_iv) := get(int_iv) - mean(get(int_iv))]
mod_global <- lm(formula = f, data = model_data_global)
global_coefs <- data.table(model='All countries',
                           name=names(mod_global$coefficients),
                           coef=mod_global$coefficients,
                           se=coef(summary(mod_global))[,2],
                           p=coef(summary(mod_global))[,4],
                           r=summary(mod_global)$r.squared,
                           r_adj=summary(mod_global)$adj.r.squared)
global_coefs[!(grep('as.factor', name)), ]
global_coefs <- rbind(global_coefs, data.table(model='All countries', name=c('R^2','Adj. R^2','N'), coef=c(summary(mod_global)$r.squared, summary(mod_global)$adj.r.squared, dim(model_data_global)[1])), fill=TRUE)

for(c in c('Algeria',gsub('as.factor\\(name\\)','',global_coefs[grep('as.factor',name), name]))) {
  if(c!='Algeria') { 
    model_data_global[name==c, country := global_coefs[name==paste0('as.factor(name)', c), coef]]
  }
  if(c=='Algeria') model_data_global[name==c, country := 0]
}
model_data_global[, intercept := global_coefs[name=='(Intercept)', coef]]
for(i in names(mod_global$residuals)) model_data_global[as.numeric(i), residual := mod_global$residuals[i]]
for(v in c('log_ldi_pc','lag5_r_size_15_24')) {
  model_data_global[, (paste0('cont_',v)) := get(v) * global_coefs[name==v, coef]]
}
model_data_global[name=='Niger' & year==2005, c('country','residual','cont_lag5_r_size_15_24','cont_log_ldi_pc','intercept')]
exp(model_data_global[name=='Niger' & year==2005, country + residual + cont_lag5_r_size_15_24 + cont_log_ldi_pc + intercept])
model_data_global[name=='Niger' & year==2005, out_rate]

## Make permutations
#fes <- c("cont_country","cont_residual","log_ldi_pc","lag5_r_size_15_24","dmean_epr","dmean_r_ldi_pc","dmean_ldi_pc_gap","dmean_gbd_mx_shocks","dmean_polity2","dmean_urbanicity")
#fes <- c('country','residual','log_ldi_pc','lag5_r_size_15_24','intercept')
fes <- c('country','log_ldi_pc','lag5_r_size_15_24','ldi_pc_gap','r_ldi_pc','gbd_mx_shocks','epr_15_24')
make_permutations <- function(fes, start_year, end_year) {
  permutations <- list()
  for(fe in 1:length(fes)) permutations[[fe]] <- c(start_year,end_year)
  permutations <- as.data.table(expand.grid(permutations))
  setnames(permutations, names(permutations), fes)
  return(permutations)
}
permutations <- make_permutations(fes=fes, start_year=0, end_year=1)
#permutations <- permutations[country==1 & intercept==1, ]
for(i in names(mod_global$residuals)) model_data_global[as.numeric(i), residual := mod_global$residuals[i]]

this_dt[name=='Benin', c('name','year','cont_country','cont_residual','cont_log_ldi_pc','cont_lag5_r_size_15_24','cont_intercept', 'lag5_r_size_15_24','with_change','without_change','diff')]

## Calculate contribution
calc_cont <- function(fe, start_year, end_year, d) {
  message(fe)
  fe_permutations <- permutations[get(fe)==end_year, ]
  other_fes <- fes[fes!=fe]
  calculate_permutation <- function(p, full_dt) {
    ## Select permutation.
    this_dt <- copy(full_dt)
    p_option <- fe_permutations[p,]
    ## Calculate contribution of thiS FE under this permutation.
    for(v in fes[!(fes %in% c('country','residual','intercept'))]) {
      if(p_option[, get(v)]==0) this_dt[, (v) := 0]
      this_dt[, (paste0('cont_',v)) := get(v) * global_coefs[name==v, coef]]
    }
    this_dt[, cont_intercept := global_coefs[grep('Intercept',name), coef]]
    for(c in c('Algeria',gsub('as.factor\\(name\\)','',global_coefs[grep('as.factor',name), name]))) {
      if(c!='Algeria') this_dt[name==c, cont_country := global_coefs[grep(c,name), coef]]
      if(c=='Algeria') this_dt[name==c, cont_country := 0]
    }
    this_dt[, cont_residual := residual]
    this_dt[, cont_country := cont_country + cont_residual + cont_intercept]
    #for(v in c('country','residual','intercept')) {
    for(v in c('country')) {
      if(p_option[, get(v)]==0) this_dt[, (paste0('cont_', v)) := 0]
    }
    iv <- 'lag5_r_size_15_24'
    int_iv <- 'log_ldi_pc'
    this_dt[, cont_lag5_r_size_15_24 := get(iv) * (global_coefs[name==iv, coef] + (global_coefs[name==paste0(iv,':',int_iv), coef] * log_ldi_pc))]
    
    #this_dt[, cont_interaction := 0]
    #this_dt[, cont_interaction_nochange := cont_interaction]
    #if(fe=='lag5_r_size_15_24') this_dt[, cont_interaction_nochange := 0]
    #if(fe=='log_ldi_pc') this_dt[, cont_interaction_nochange := get(iv) * (global_coefs[name==iv, coef])]
    ## Calculate total difference in prediction attributable to this covariate.
    this_dt[, with_change := apply(.SD, 1, sum, na.rm=T), .SDcols=c(paste0('cont_',fe),paste0('cont_',other_fes))]
    this_dt[, without_change := apply(.SD, 1, sum, na.rm=T), .SDcols=c(paste0('cont_',other_fes))]
    this_dt[, diff := exp(with_change) - exp(without_change)]
    return(this_dt)
  }
  #total_cont <- mean(unlist(lapply(1:dim(fe_permutations)[1], calculate_permutation, full_dt=d)))
  all_diffs <- as.data.table(rbind.fill(lapply(1:dim(fe_permutations)[1], calculate_permutation, full_dt=d)))
  #total_cont <- data.table(cov=fe, cont=total_cont)
  all_diffs <- all_diffs[, list(cont=mean(diff)), by=c('name','year')]
  all_diffs[, fe := fe]
  return(all_diffs)
}
all_conts <- rbindlist(lapply(c(fes), calc_cont, start_year=0, end_year=1, d=model_data_global))
test <- all_conts[, list(decomp_out_rate=sum(cont)), by=c('name','year')]
model_data_global$decomp_out_rate <- NULL
model_data_global <- merge(model_data_global, test, by=c('name','year'))
model_data_global[name=='Ethiopia', c('name','year','out_rate','decomp_out_rate')]
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Shapley_decomp_comparison.pdf'), width = 12, height = 8)
ggplot() +
  geom_point(data=model_data_global[decomp_out_rate<100 & decomp_out_rate>-5,],
             aes(x=decomp_out_rate,
                 y=out_rate)) +
  geom_abline(slope=1, intercept=0) + 
  ggtitle(paste0('Showing ', length(model_data_global[decomp_out_rate<100 & decomp_out_rate>-5, decomp_out_rate]), '/', length(model_data_global[, decomp_out_rate]), ' country-years.')) + 
  theme_minimal()
dev.off()


## Predict on all permutations.
calculate_contribution <- function(fe, fes, start_year, end_year, coefs, d) {
  ## Grab all permutations where this fixed effect is changing (2010) and calculate difference vs. difference if it had not changed (1990).
  ## The difference of these two differences is the "contribution" of change in that fixed effect WITHIN this permutation (though this
  ## difference seems to be identical across permutations).
  message(paste0('Calculating contribution from ', fe, '...'))
  fe_permutations <- permutations[get(fe)==end_year, ]
  other_fes <- fes
  other_fes <- other_fes[other_fes!=fe]
  calculate_permutation <- function(p, dt, coefs) {
    
    ## Select permutation.
    this_dt <- copy(dt)
    p_option <- fe_permutations[p,]
    
    ## Assign values to be added from all effects besides the target effect.
    for(other_fe in other_fes) {
      if(!(other_fe %in% c('residual'))) {
        ## Assign relevant fixed effect values.
        if(p_option[, get(other_fe)]==end_year) this_dt[, (paste0('p_',other_fe)) := get(paste0(other_fe,'_', end_year)) * coefs[name==other_fe, coef]]
        if(p_option[, get(other_fe)]==start_year) this_dt[, (paste0('p_',other_fe)) := get(paste0(other_fe,'_', start_year)) * coefs[name==other_fe, coef]]
      }
      if(other_fe=='residual') {
        ## Assign relevant value for residual.
        if(p_option[, residual]==end_year) this_dt[, p_residual := get(paste0('residual_',end_year))] ## MAKE SURE RESIDUAL IS IN LOGIT SPACE - it comes in normal space from INLA object.
        if(p_option[, residual]==start_year) this_dt[, p_residual := get(paste0('residual_',start_year))]
      }
    }
    
    ## Generate full prediction for this permutation based on whether FE value stayed the same or changed over the period.
    ## Assign target FE value based on change (2010 value) or no change (1990 value), and then add in all other effects.
    if(!(fe %in% c('residual'))) {
      this_dt[, (paste0('p_with_change_',p)) := (get(paste0(fe,'_',end_year)) * coefs[name==fe, coef])]
      this_dt[, (paste0('p_without_change_',p)) := (get(paste0(fe,'_',start_year)) * coefs[name==fe, coef])]
    }
    if(fe=='residual') {
      this_dt[, (paste0('p_with_change_',p)) := get(paste0('residual_',end_year))]
      this_dt[, (paste0('p_without_change_',p)) := get(paste0('residual_',start_year))]
    }
    
    ## Add intercept and random effects.
    for(c in c('Algeria',gsub('as.factor\\(name\\)','',coefs[grep('as.factor',name), name]))) {
      if(c=='Algeria') this_dt[name==c, country_int := 0]
      if(c!='Algeria') this_dt[name==c, country_int := coefs[name==paste0('as.factor(name)',c), coef]]
    }
    this_dt[, (paste0('p_with_change_',p)) := get((paste0('p_with_change_',p))) + coefs[name=='(Intercept)', coef] + country_int]
    this_dt[, (paste0('p_without_change_',p)) := get((paste0('p_without_change_',p))) + coefs[name=='(Intercept)', coef] + country_int]
    
    ## This change (2010 prediction based just on FE of interest - 1990 prediction) is the same across all permutations... 
    ## But then we are just adding a constant to both sides derived from this specific permutation of all other effects...?
    #message(this_dt[, get((paste0('p_with_change_',p)))][1])
    for(other_fe in other_fes) {
      this_dt[, (paste0('p_with_change_',p)) := get((paste0('p_with_change_',p))) + get(paste0('p_',other_fe))]
      this_dt[, (paste0('p_without_change_',p)) := get((paste0('p_without_change_',p))) + get(paste0('p_',other_fe))]
    }
    
    ## Generate difference in full prediction for this permutation attributable to change in this FE value over the period.
    ## The difference attributable to this effect in this permutation needs to be calculated in normal space. This is how we handle non-linearities.
    ## If we decompose life expectancy, here is where you would want to convert both scenarios in this permutation to normal space, 
    ## calculate life expectancies, and return the difference.
    this_dt[, diff := exp(get(paste0('p_with_change_',p))) - exp(get(paste0('p_without_change_',p)))]
    this_dt[, p_with_change := get(paste0('p_with_change_',p))]
    this_dt[, p_without_change := get(paste0('p_without_change_',p))]
    this_dt <- this_dt[, c('name', 'p_with_change', 'p_without_change', 'diff')]
    this_dt[, p := p]
    return(this_dt)
    
  }
  
  message('Calculating all permutations...')
  all_diffs <- as.data.table(rbind.fill(lapply(1:dim(fe_permutations)[1], calculate_permutation, dt=d, coefs=coefs)))
  ## As this is a Shapley decomposition, here is where we "average over" potential path dependencies (i.e. all the different permutations).
  ## A more complex generalized decomposition could be used, such as g-computation (actually estimate all the path dependencies, decompose
  ## direct/indirect change attributable via bootstrap and stochastic simulation through periods.
  all_diffs <- all_diffs[, list(cont=mean(diff)), by=c('name')]
  all_diffs[, fe := fe]
  all_diffs[, year := end_year]
  return(all_diffs)
  
}

## Reshape data and make all changes
fes <- c('residual','log_ldi_pc','lag5_r_size_15_24','ldi_pc_gap','r_ldi_pc','gbd_mx_shocks','epr_15_24')
make_permutations <- function(fes, start_year, end_year) {
  permutations <- list()
  for(fe in 1:length(fes)) permutations[[fe]] <- c(start_year,end_year)
  permutations <- as.data.table(expand.grid(permutations))
  setnames(permutations, names(permutations), fes)
  return(permutations)
}
permutations <- make_permutations(fes=fes, start_year=2000, end_year=2005)
#permutations <- permutations[country==1 & intercept==1, ]
for(i in names(mod_global$residuals)) model_data_global[as.numeric(i), residual := mod_global$residuals[i]]
decomp_data <- dcast(model_data_global, name ~ year, value.var=c('out_rate',fes))
all_contributions <- rbindlist(lapply(fes, calculate_contribution,
                                      fes=fes,
                                      start_year=2000,
                                      end_year=2005,
                                      coefs=global_coefs,
                                      d=decomp_data))
test <- all_contributions[, list(decomp_change=sum(cont)), by=c('name','year')]
decomp_data[, obs_change := out_rate_2005 - out_rate_2000]
decomp_data <- merge(decomp_data, test, by=c('name'))
decomp_data <- decomp_data[name %in% c('Algeria',gsub('as.factor\\(name\\)','',coefs[grep('as.factor',name), name]))]

pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Shapley_change_2000_2005_decomp_comparison.pdf'), width = 12, height = 8)
ggplot() +
  geom_point(data=decomp_data,
             aes(x=decomp_change,
                 y=obs_change)) +
  geom_abline(slope=1, intercept=0) + 
  theme_minimal()
dev.off()

plot_data <- copy(all_contributions)
plot_data <- merge(plot_data, unique(model_data_global[, c('name','gbd_super_region')]), by='name')
plot_data <- merge(plot_data, decomp_data[, c('name','obs_change')])
plot_data[, country_year := paste0(name,'_',year)]
plot_data_obs <- unique(plot_data[, c('country_year','obs_change')])
plot_data_obs[, country_year_f := factor(country_year, levels=plot_data_obs$country_year[order(plot_data_obs[, obs_change])])]
plot_data[, country_year_f := factor(country_year, levels=plot_data_obs$country_year[order(plot_data_obs[, obs_change])])]
#plot_data_obs[, region_f := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Shapley_change_2000_2005_test_Figure_3_', Sys.Date(), '.pdf'), width = 20, height = 12)
ggplot() +
  geom_bar(data=plot_data[!is.na(obs_change),],
           aes(x=country_year_f,
               y=cont,
               fill=fe),
           color='black',
           stat='identity',
           width=0.7) + 
  geom_point(data=plot_data_obs[!is.na(obs_change),],
             aes(x=country_year_f,
                 y=obs_change),
             size=3) + 
  # geom_errorbar(data=plot_data[cont_interaction_lower >= 0 & cont_interaction_mean >= 0.75,],
  #               aes(x=country_year_f, ymin=cont_interaction_lower, ymax=cont_interaction_upper), width=.4) + 
  theme_minimal() + 
  scale_fill_manual(name='GBD Super Region', values=brewer.pal(7,'Spectral')) +
  labs(y='Total change in out-migration/thousand', x='', title='Total change in out-migration/thousand attributable to each covariate, 2000-2005.') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot_data_regions <- merge(plot_data, model_data_global[year==2005, c('name','total_pop','gbd_region')])
plot_data_regions <- plot_data_regions[, list(cont=weighted.mean(cont, total_pop, na.rm=TRUE)), by=c('gbd_region','fe')]
plot_data_regions_obs <- merge(plot_data, model_data_global[year==2005, c('name','total_pop','gbd_region')])
plot_data_regions_obs <- plot_data_regions_obs[, list(obs_change=weighted.mean(obs_change, total_pop, na.rm=TRUE)), by=c('gbd_region')]
plot_data_regions_obs[, region_f := factor(gbd_region, levels=plot_data_regions_obs$gbd_region[order(plot_data_regions_obs[, obs_change])])]
plot_data_regions[, region_f := factor(gbd_region, levels=plot_data_regions_obs$gbd_region[order(plot_data_regions_obs[, obs_change])])]
ggplot() +
  geom_bar(data=plot_data_regions,
           aes(x=region_f,
               y=cont,
               fill=fe),
           color='black',
           stat='identity',
           width=0.7) + 
  geom_point(data=plot_data_regions_obs[!is.na(obs_change),],
             aes(x=region_f,
                 y=obs_change),
             size=3) + 
  # geom_errorbar(data=plot_data[cont_interaction_lower >= 0 & cont_interaction_mean >= 0.75,],
  #               aes(x=country_year_f, ymin=cont_interaction_lower, ymax=cont_interaction_upper), width=.4) + 
  theme_minimal() + 
  scale_fill_manual(name='GBD Super Region', values=brewer.pal(7,'Set1')) +
  labs(y='Total change in out-migration/thousand', x='', title='Total change in out-migration/thousand attributable to each covariate, 2000-2005.') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
dev.off()


## Just do exp(growth + stuff) - exp(stuff)
for(c in c('Algeria',gsub('as.factor\\(name\\)','',global_coefs[grep('as.factor',name), name]))) {
  if(c=='Algeria') model_data_global[name==c, country_int := 0]
  if(c!='Algeria') model_data_global[name==c, country_int := global_coefs[name==paste0('as.factor(name)',c), coef]]
}
model_data_global[, intercept := global_coefs[name=='(Intercept)', coef]]
for(i in names(mod_global$residuals)) model_data_global[as.numeric(i), residual := mod_global$residuals[i]]
for(v in fes <- c('log_ldi_pc','ldi_pc_gap','r_ldi_pc','gbd_mx_shocks','epr_15_24')) {
  model_data_global[, (paste0('cont_',v)) := get(v) * global_coefs[name==v, coef]]
}
model_data_global[, cont_interaction := get(iv) * (global_coefs[name==iv, coef] + (global_coefs[name==paste0(iv,':',int_iv), coef] * log_ldi_pc))]




for(v in c(int_iv,paste0('',c('r_ldi_pc','ldi_pc_gap','gbd_mx_shocks','polity2','urbanicity','epr_15_24','log_ldi_pc')))) {
  model_data_global[, (paste0('cont_',v)) := get(v) * global_coefs[name==v, coef]]
}
for(c in c('Algeria',gsub('as.factor\\(name\\)','',global_coefs[grep('as.factor',name), name]))) {
  if(c!='Algeria') { 
    model_data_global[name==c, cont_country := global_coefs[name==paste0('as.factor(name)', c), coef]]
  }
  if(c=='Algeria') model_data_global[name==c, cont_country := 0]
}
model_data_global[, cont_intercept := global_coefs[name=='(Intercept)', coef]]
for(i in names(mod_global$residuals)) model_data_global[as.numeric(i), residual := mod_global$residuals[i]]
plot_data <- copy(model_data_global)
library(arm)
sims <- sim(mod_global, 1000)
sims <- as.data.table(sims@coef)
sims[, draw := 1:1000]
sims[, index := 1]
draws <- dcast(sims, index ~ draw, value.var=c('lag5_r_size_15_24','lag5_r_size_15_24:log_ldi_pc'))
for(draw in 1:500) {
  plot_data[, (paste0('cont_interaction_',draw)) := get(iv) * (draws[, get(paste0(iv,'_',draw))] + (draws[, get(paste0(iv,':',int_iv,'_',draw))] * log_ldi_pc))]
  plot_data[, (paste0('total_growth_contribution_',draw)) := exp(cont_country + cont_intercept + residual + get(paste0('cont_interaction_',draw)) + cont_log_ldi_pc +
                                                                 cont_ldi_pc_gap + cont_r_ldi_pc + cont_gbd_mx_shocks + cont_epr_15_24) -
                                                             exp(cont_country + cont_intercept + residual + cont_log_ldi_pc +
                                                                 cont_ldi_pc_gap + cont_r_ldi_pc + cont_gbd_mx_shocks + cont_epr_15_24)]
  plot_data[, (paste0('abs_total_growth_contribution_',draw)) := (get(paste0('total_growth_contribution_',draw))/1000) * total_pop]
}
plot_data[, total_growth_contribution_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^total_growth_contribution_", names(plot_data))]
plot_data[, total_growth_contribution_upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^total_growth_contribution_", names(plot_data))]
plot_data[, total_growth_contribution_lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^total_growth_contribution_", names(plot_data))]
plot_data[, total_growth_contribution_pop_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data))]
plot_data[, total_growth_contribution_pop_upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data))]
plot_data[, total_growth_contribution_pop_lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data))]
plot_data[, total_growth_contribution_pop := (total_growth_contribution_mean/1000) * total_pop]
plot_data[, country_year := paste0(name,'_',year)]
plot_data[, country_year_f := factor(country_year, levels=plot_data$country_year[order(plot_data[, total_growth_contribution_mean])])]
plot_data[, region_f := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]
ggplot() + 
  geom_point(data=plot_data,
             aes(x=total_growth_contribution_pop,
                 y=total_growth_contribution_pop_mean)) + geom_abline(slope = 1, intercept = 0) + theme_minimal()
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/by_country_year_absolute_Figure_3_', Sys.Date(), '.pdf'), width = 12, height = 8)
for(y in c(1990,1995,2000,2005)) { 
plot_data2 <- copy(plot_data[year==y, ])
#plot_data[, country_year_f := factor(country_year, levels=plot_data$country_year[order(plot_data[, cont_interaction])])]
plot_data2[, region_f := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]
#plot_data2[, total_growth_contribution_pop := (total_growth_contribution/1000) * total_pop]
plot_data2[, country_f := factor(name, levels=plot_data2$name[order(plot_data2[, total_growth_contribution_pop])])]
plot_data3 <- copy(plot_data2)
plot_data3[, country_f := factor(name, levels=plot_data3$name[order(plot_data3[, total_growth_contribution])])]

# plot_data3 <- plot_data3[total_growth_contribution >= 0, ]
# plot_data3[, plot_diff := out_rate - total_growth_contribution]
# plot_data3 <- melt(plot_data3, id.vars = c('country_f','region_f','out_rate'), measure.vars = c('plot_diff','total_growth_contribution'), variable.name = 'bar_vals', value.name = 'est_out')
this_gg <- ggplot() +
  geom_bar(data=plot_data3[!is.na(total_growth_contribution) & total_growth_contribution_lower >= 0],
           aes(x=country_f,
               y=total_growth_contribution,
               fill=region_f),
           color='black',
           stat='identity') + 
  # geom_point(data=plot_data3[!is.na(est_out) & est_out >= 0],
  #            aes(x=country_f,
  #                y=out_rate),
  #            size=3) + 
  geom_errorbar(data=plot_data3[total_growth_contribution_lower >= 0,],
                aes(x=country_f, ymin=total_growth_contribution_lower, ymax=total_growth_contribution_upper), width=.4) +
  theme_minimal() + 
  scale_alpha_discrete(guide=FALSE) + 
  scale_fill_manual(name='GBD Super Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) +
  labs(y='Total out-migration/thousand attributable to growth', x='', title=paste0('Total out-migration/thousand attributable to growth for all countries where this contribution is positive, ', y, '.')) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
print(this_gg)
# plot_data2 <- plot_data2[total_growth_contribution_pop >= 0, ]
# plot_data2[, plot_diff := (out_rate/1000)*total_pop - total_growth_contribution_pop]
# plot_data2 <- melt(plot_data2, id.vars = c('country_f','region_f','out_rate'), measure.vars = c('plot_diff','total_growth_contribution_pop'), variable.name = 'bar_vals', value.name = 'est_out')
this_gg2 <- ggplot() +
  geom_bar(data=plot_data2[!is.na(total_growth_contribution_pop) & total_growth_contribution_pop_lower >= 0],
           aes(x=country_f,
               y=total_growth_contribution_pop,
               fill=region_f),
           color='black',
           stat='identity') + 
  # geom_point(data=plot_data2[name != 'Pakistan' & !is.na(total_growth_contribution) & total_growth_contribution >= 0],
  #            aes(x=country_f,
  #                y=(out_rate/1000)*total_pop),
  #            size=3) + 
  geom_errorbar(data=plot_data2[total_growth_contribution_lower >= 0,],
                aes(x=country_f, ymin=total_growth_contribution_pop_lower, ymax=total_growth_contribution_pop_upper), width=.4) +
  theme_minimal() + 
  scale_alpha_discrete(guide=FALSE) + 
  scale_fill_manual(name='GBD Super Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) +
  labs(y='Total out-migrants attributable to growth', x='', title=paste0('Total number of out-migrants attributable to growth for all countries where this contribution is positive, ', y,'.')) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
print(this_gg2)
}
dev.off()
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/1990_2010_aggregated_absolute_Figure_3_', Sys.Date(), '.pdf'), width = 12, height = 8)
target_period <- c(1990,1995,2000,2005)
year_title <- '1990-2010'
plot_drops <- ''
  for(q in rev(seq(0.1,1,0.1))) {
    log_ldi_pc_q <- quantile(plot_data[, log_ldi_pc], p=q, na.rm=TRUE)
    message(round(log_ldi_pc_q, 3))
    plot_data[log_ldi_pc <= log_ldi_pc_q, log_ldi_pc_group := as.character(round(log_ldi_pc_q,2))]
  }
plot_data2 <- copy(plot_data[year %in% target_period & !(name %in% plot_drops), ])
plot_data2 <- plot_data2[, lapply(.SD, sum, na.rm=TRUE), by=c('name','gbd_region'), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2)) ]
plot_data2[, total_growth_contribution_pop_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, total_growth_contribution_pop_upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, total_growth_contribution_pop_lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, region_f := factor(gbd_region)]
plot_data2[, country_f := factor(name, levels=plot_data2$name[order(plot_data2[, total_growth_contribution_pop_mean])])]
this_gg2 <- ggplot() +
  geom_bar(data=plot_data2[!is.na(total_growth_contribution_pop_mean) & total_growth_contribution_pop_lower >= 0],
           aes(x=country_f,
               y=total_growth_contribution_pop_mean,
               fill=region_f),
           color='black',
           stat='identity') + 
  # geom_point(data=plot_data2[name != 'Pakistan' & !is.na(total_growth_contribution) & total_growth_contribution >= 0],
  #            aes(x=country_f,
  #                y=(out_rate/1000)*total_pop),
  #            size=3) + 
  geom_errorbar(data=plot_data2[total_growth_contribution_pop_lower >= 0,],
                aes(x=country_f, ymin=total_growth_contribution_pop_lower, ymax=total_growth_contribution_pop_upper), width=.4) +
  theme_minimal() + 
  scale_alpha_discrete(guide=FALSE) + 
  scale_fill_manual(name='GBD Super Region', values=brewer.pal(12,'Set3')) +
  labs(y='Total out-migrants attributable to growth', x='', title=paste0('Total number of out-migrants attributable to growth for all countries where this contribution is positive, ', year_title,'.')) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
print(this_gg2)
plot_data2 <- copy(plot_data[year %in% target_period & !(name %in% plot_drops), ])
plot_data2 <- plot_data2[, lapply(.SD, sum, na.rm=TRUE), by=c('gbd_region'), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2)) ]
plot_data2[, total_growth_contribution_pop_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, total_growth_contribution_pop_upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, total_growth_contribution_pop_lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, region_f := factor(gbd_region, levels=plot_data2$gbd_region[order(plot_data2[, total_growth_contribution_pop_mean])])]
this_gg2 <- ggplot() +
  geom_bar(data=plot_data2[!is.na(total_growth_contribution_pop_mean)],
           aes(x=region_f,
               y=total_growth_contribution_pop_mean,
               fill=region_f),
           color='black',
           stat='identity') + 
  # geom_point(data=plot_data2[name != 'Pakistan' & !is.na(total_growth_contribution) & total_growth_contribution >= 0],
  #            aes(x=country_f,
  #                y=(out_rate/1000)*total_pop),
  #            size=3) + 
  geom_errorbar(data=plot_data2,
                aes(x=region_f, ymin=total_growth_contribution_pop_lower, ymax=total_growth_contribution_pop_upper), width=.4) +
  theme_minimal() + 
  scale_alpha_discrete(guide=FALSE) + 
  scale_fill_manual(name='GBD Super Region', values=brewer.pal(12,'Set3')) +
  labs(y='Total out-migrants attributable to growth', x='', title=paste0('Total number of out-migrants attributable to growth for all GBD regions, ', year_title,'.')) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
print(this_gg2)
## By LDI quantile
plot_data2 <- copy(plot_data[!is.na(log_ldi_pc_group) & year %in% target_period & !(name %in% plot_drops), ])
plot_data2 <- plot_data2[, lapply(.SD, sum, na.rm=TRUE), by=c('log_ldi_pc_group'), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2)) ]
plot_data2[, total_growth_contribution_pop_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, total_growth_contribution_pop_upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, total_growth_contribution_pop_lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, region_f := factor(log_ldi_pc_group, levels=plot_data2$log_ldi_pc_group[order(plot_data2[, as.numeric(log_ldi_pc_group)])])]
this_gg2 <- ggplot() +
  geom_bar(data=plot_data2[!is.na(total_growth_contribution_pop_mean)],
           aes(x=region_f,
               y=total_growth_contribution_pop_mean,
               fill=region_f),
           color='black',
           stat='identity') + 
  # geom_point(data=plot_data2[name != 'Pakistan' & !is.na(total_growth_contribution) & total_growth_contribution >= 0],
  #            aes(x=country_f,
  #                y=(out_rate/1000)*total_pop),
  #            size=3) + 
  geom_errorbar(data=plot_data2,
                aes(x=region_f, ymin=total_growth_contribution_pop_lower, ymax=total_growth_contribution_pop_upper), width=.4) +
  theme_minimal() + 
  scale_alpha_discrete(guide=FALSE) + 
  scale_fill_manual(name='LDI/pc groups', values=brewer.pal(12,'Set3')) +
  labs(y='Total out-migrants attributable to growth', x='', title=paste0('Total number of out-migrants attributable to growth for all LDI/pc groups, ', year_title,'.')) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
print(this_gg2)
dev.off()

## Make absolute table
plot_data2 <- copy(plot_data[year %in% target_period & !(name %in% plot_drops), ])
plot_data2 <- plot_data2[, lapply(.SD, sum, na.rm=TRUE), by=c('gbd_region'), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2)) ]
plot_data2[, total_growth_contribution_pop_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, total_growth_contribution_pop_upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, total_growth_contribution_pop_lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, region_f := factor(gbd_region, levels=plot_data2$gbd_region[order(plot_data2[, total_growth_contribution_pop_mean])])]

wb_ids <- fread("C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/wb_gbd_ids.csv")


plot_data <- copy(model_data_global)
for(draw in 1:1000) {
  plot_data[, (paste0('cont_interaction_',draw)) := (exp(get(iv) * (draws[, get(paste0(iv,'_',draw))] + (draws[, get(paste0(iv,':',int_iv,'_',draw))] * log_ldi_pc)))-1)*100]
}
plot_data[, cont_interaction_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^cont_interaction_", names(plot_data))]
plot_data[, cont_interaction_upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^cont_interaction_", names(plot_data))]
plot_data[, cont_interaction_lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^cont_interaction_", names(plot_data))]
plot_data[, country_year := paste0(name,'_',year)]
plot_data[, country_year_f := factor(country_year, levels=plot_data$country_year[order(plot_data[, cont_interaction])])]
plot_data[, region_f := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]

library(dplyr)
library(forcats)
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/v2_relative_Figure_3_', Sys.Date(), '.pdf'), width = 12, height = 8)
for(y in c(1990,1995,2000,2005)) {
plot_data2 <- copy(plot_data[year==y, ])
plot_data2[, country_f := factor(name, levels=plot_data2$name[order(plot_data2[, cont_interaction])])]
#plot_data[, country_year_f := factor(country_year, levels=plot_data$country_year[order(plot_data[, cont_interaction])])]
plot_data2[, region_f := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]
this_gg <- ggplot() +
  geom_bar(data=plot_data2[!is.na(cont_interaction_mean) & cont_interaction_lower >= 0,],
           aes(x=country_f,
               y=(exp(cont_interaction)-1)*100,
               fill=region_f),
           color='black',
           stat='identity') + 
  geom_errorbar(data=plot_data2[!is.na(cont_interaction) & cont_interaction_lower >= 0,],
                aes(x=country_f, ymin=cont_interaction_lower, ymax=cont_interaction_upper), width=.4) +
  theme_minimal() + 
  scale_fill_manual(name='GBD Super Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) +
  labs(y='Percent increase in out-migration/thousand attributable to growth', x='', title=paste0('Percent increase in out-migration/thousand attributable to growth for all countries where this contribution is positive, ', y, '.')) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
print(this_gg)
}
dev.off()







                
                
plot_data <- copy(all_conts[fe=='lag5_r_size_15_24',])
plot_data <- merge(plot_data, unique(model_data_global[, c('name','gbd_super_region')]), by='name')
plot_data[, cont_interaction_mean := cont]
plot_data[, country_year := paste0(name,'_',year)]
plot_data[, country_year_f := factor(country_year, levels=plot_data$country_year[order(plot_data[, cont_interaction_mean])])]
plot_data[, region_f := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Shapley_test_Figure_3_', Sys.Date(), '.pdf'), width = 12, height = 8)
# ggplot() +
#   geom_bar(data=plot_data[cont >= 10 & cont < 100,],
#            aes(x=country_year_f,
#                y=cont,
#                fill=region_f),
#            color='black',
#            stat='identity') + 
#   # geom_errorbar(data=plot_data[cont_interaction_lower >= 0 & cont_interaction_mean >= 0.75,],
#   #               aes(x=country_year_f, ymin=cont_interaction_lower, ymax=cont_interaction_upper), width=.4) + 
#   theme_minimal() + 
#   scale_fill_manual(name='GBD Super Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) +
#   labs(y='Total out-migration/thousand attributable to growth', x='', title='Total out-migration/thousand attributable to growth for all countries where this contribution is positive, 2005.') + 
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot_data2 <- copy(plot_data[year==2005, ])
plot_data2[, country_f := factor(name, levels=plot_data2$name[order(plot_data2[, cont_interaction_mean])])]
#plot_data[, country_year_f := factor(country_year, levels=plot_data$country_year[order(plot_data[, cont_interaction])])]
plot_data2[, region_f := factor(gbd_super_region, levels = c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]
ggplot() +
  geom_bar(data=plot_data2[cont >= 0 & cont < 100,],
           aes(x=country_f,
               y=cont,
               fill=region_f),
           color='black',
           stat='identity') + 
  # geom_errorbar(data=plot_data2[cont_interaction_lower >= 0,],
  #               aes(x=country_f, ymin=cont_interaction_lower, ymax=cont_interaction_upper), width=.4) + 
  theme_minimal() + 
  scale_fill_manual(name='GBD Super Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) +
  labs(y='Total out-migration/thousand attributable to growth', x='', title='Total out-migration/thousand attributable to growth for all countries where this contribution is positive, 2005.') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
dev.off()


model_data_global[name=='Ethiopia' & year==2005, c('country_int','intercept','residual','cont_interaction',
                                                   'cont_log_ldi_pc','cont_ldi_pc_gap','cont_r_ldi_pc','cont_gbd_mx_shocks',
                                                   'cont_epr_15_24')]
model_data_global[name=='Ethiopia' & year==2005, exp(country_int + intercept + residual + 0 + cont_log_ldi_pc +
                            cont_ldi_pc_gap + cont_r_ldi_pc + cont_gbd_mx_shocks + cont_epr_15_24)]
model_data_global[name=='Ethiopia' & year==2005, exp(country_int + intercept + residual + cont_interaction + cont_log_ldi_pc +
                                                       cont_ldi_pc_gap + cont_r_ldi_pc + cont_gbd_mx_shocks + cont_epr_15_24)]
model_data_global[name=='Ethiopia' & year==2005, (global_coefs[name==iv, coef] + (global_coefs[name==paste0(iv,':',int_iv), coef] * log_ldi_pc))]


                