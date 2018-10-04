library(rsq)
library(relaimpo)
library(plyr)
library(data.table)
library(ggplot2)
library(olsrr)
library(interplot)
library(RColorBrewer)

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
ilo_epr <- fread('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/ILOSTAT_epr.csv')
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
ilo_unemp <- fread("C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/API_SL.UEM.1524.ZS_DS2_en_csv_v2_10034482/API_SL.UEM.1524.ZS_DS2_en_csv_v2_10034482.csv")
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
land <- fread("C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/FAOSTAT_data_arable_land.csv")
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
saveRDS(model_data_global, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/model_data_global.RDS')
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

## MAKE FIGURE 2 AND INTERACTION COEFFICIENT TABLE
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_2_', Sys.Date(), '.pdf'), width = 20, height = 10)
for(q in c(7,4,2,0)) {
  model_data_global[original_lag5_r_size_15_24 <= q, growth_group := as.character(round(q,0))]
}
global_interplot <- interplot(m = mod_global, var1 = iv, var2 = int_iv) +
  geom_hline(yintercept = 0, color='red') +
  geom_vline(xintercept = 0, color='red') +
  #ggtitle('Figure 2. Coefficient for growth on out-migration based on level of income per capita.') +
  ylab(paste0('Coefficient of growth on out-migration')) + xlab(paste0('Income per capita (mean-centered)')) + theme_minimal()
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
  scale_alpha_manual(values=c(.2,.5,.8,1),name='Growth rate',labels=c('< 0','0-2','2-4','4+')) + 
  scale_fill_manual(name='Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) +
  guides(fill = guide_legend(override.aes = list(size = 10)))
dev.off()
coef_data <- data.table(global_interplot$data)
setnames(coef_data, 'fake', 'log_ldi_pc')
write.csv(coef_data, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_2_coef_data.csv', row.names=FALSE)
scatter_data <- copy(model_data_global)
scatter_data[, predicted_coef := log_ldi_pc * mod_global$coefficients[paste0(iv,':',int_iv)] + mod_global$coefficients[iv]]
scatter_data <- scatter_data[, c('name','year','region_f','original_lag5_r_size_15_24','log_ldi_pc','predicted_coef')]
write.csv(scatter_data, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_2_scatter_data.csv', row.names=FALSE)

## MAKE FIGURE 3 AND ABSOLUTE CONTRIBUTIONS TABLE
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
saveRDS(global_coefs, paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/stage3_models_', iv, '_', Sys.Date(), '.RDS'))

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

## FIGURE 3 - BY COUNTRY, 1990-2010 (ONLY POSITIVE CONTRIBUTIONS)
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_3_', Sys.Date(), '.pdf'), width = 12, height = 8)
target_period <- c(1990,1995,2000,2005)
year_title <- '1990-2010'
plot_drops <- ''
for(q in rev(seq(0.1,1,0.1))) {
  log_ldi_pc_q <- quantile(plot_data[, log_ldi_pc], p=q, na.rm=TRUE)
  message(round(log_ldi_pc_q, 3))
  plot_data[log_ldi_pc <= log_ldi_pc_q, log_ldi_pc_group := as.character(round(log_ldi_pc_q,2))]
}
plot_data2 <- copy(plot_data[year %in% target_period & !(name %in% plot_drops), ])
plot_data2[, total_growth_contribution_pop_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
#plot_data2 <- plot_data2[total_growth_contribution_pop_mean>=0, lapply(.SD, sum, na.rm=TRUE), by=c('name','gbd_region'), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2)) ]
plot_data2 <- plot_data2[, lapply(.SD, sum, na.rm=TRUE), by=c('name','gbd_super_region'), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2)) ]
plot_data2[, total_growth_contribution_pop_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, total_growth_contribution_pop_upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2[, total_growth_contribution_pop_lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
plot_data2 <- plot_data2[total_growth_contribution_pop_mean >= 10000]
plot_data2[, region_f := factor(gbd_super_region, levels = unique(plot_data2$gbd_super_region))]
plot_data2[, country_f := factor(name, levels=plot_data2$name[order(plot_data2[, total_growth_contribution_pop_mean])])]
plot_data2 <- plot_data2[order(-total_growth_contribution_pop_mean)]
plot_data2 <- plot_data2[1:20, ]
absolute_gg3 <- ggplot() +
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
  scale_fill_manual(name='Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) +
  labs(y='Total net out-migrants', x='', title=paste0('Total number of net out-migrants attributable to growth for all countries where this net contribution is positive, ', year_title,'.')) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
print(absolute_gg3)
dev.off()
summarize_growth(plot_data2, var_name = 'country_contribution')
plot_data2 <- plot_data2[order(-total_growth_contribution_pop_mean)]
write.csv(plot_data2[, c('country_f','total_growth_contribution_pop_mean','total_growth_contribution_pop_lower','total_growth_contribution_pop_upper')], 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_3_absolute_data.csv', row.names=FALSE)

## FIGURE 3 - BY COUNTRY, 1990-2010 (RELATIVE CONTRIBUTIONS)
saveRDS(draws, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/draws.RDS')
for(draw in 1:500) {
  plot_data[, (paste0('cont_interaction_',draw)) := get(iv) * (draws[, get(paste0(iv,'_',draw))] + (draws[, get(paste0(iv,':',int_iv,'_',draw))] * log_ldi_pc))]
  plot_data[, (paste0('total_growth_contribution_',draw)) := exp(cont_country + cont_intercept + residual + get(paste0('cont_interaction_',draw)) + cont_log_ldi_pc +
                                                                   cont_ldi_pc_gap + cont_r_ldi_pc + cont_gbd_mx_shocks + cont_epr_15_24) -
              exp(cont_country + cont_intercept + residual + cont_log_ldi_pc +
                    cont_ldi_pc_gap + cont_r_ldi_pc + cont_gbd_mx_shocks + cont_epr_15_24)]
  plot_data[, (paste0('abs_total_growth_contribution_',draw)) := (get(paste0('total_growth_contribution_',draw))/1000) * total_pop]
  
  plot_data[, (paste0('total_migrants_with_growth_',draw)) := (exp(cont_country + cont_intercept + residual + get(paste0('cont_interaction_',draw)) + cont_log_ldi_pc +
                                                                   cont_ldi_pc_gap + cont_r_ldi_pc + cont_gbd_mx_shocks + cont_epr_15_24) / 1000) * total_pop]
  plot_data[, (paste0('total_migrants_without_growth_',draw)) := (exp(cont_country + cont_intercept + residual + cont_log_ldi_pc +
                                                                   cont_ldi_pc_gap + cont_r_ldi_pc + cont_gbd_mx_shocks + cont_epr_15_24) / 1000) * total_pop]
}

pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/relative_Figure_3_', Sys.Date(), '.pdf'), width = 12, height = 8)
target_period <- c(1990,1995,2000,2005)
year_title <- '1990-2010'
plot_drops <- ''
plot_data2 <- copy(plot_data[year %in% target_period & !(name %in% plot_drops), ])
plot_data2 <- plot_data2[, lapply(.SD, sum, na.rm=TRUE), by=c('name','gbd_super_region'), .SDcols=c(grep("^total_migrants_with_growth_", names(plot_data2)),
                                                                                                    grep("^total_migrants_without_growth_", names(plot_data2))) ]
for(draw in 1:500) {
  plot_data2[, (paste0('total_relative_growth_contribution_',draw)) := (get(paste0('total_migrants_with_growth_',draw)) / get(paste0('total_migrants_without_growth_',draw)) - 1) * 100]
}
plot_data2[, total_relative_growth_contribution_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^total_relative_growth_contribution_", names(plot_data2))]
plot_data2[, total_relative_growth_contribution_upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^total_relative_growth_contribution_", names(plot_data2))]
plot_data2[, total_relative_growth_contribution_lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^total_relative_growth_contribution_", names(plot_data2))]
plot_data2[, region_f := factor(gbd_super_region, levels = unique(plot_data2$gbd_super_region))]
plot_data2[, country_f := factor(name, levels=plot_data2$name[order(plot_data2[, total_relative_growth_contribution_mean])])]
plot_data2 <- plot_data2[order(-total_relative_growth_contribution_mean)]
plot_data2 <- plot_data2[1:20, ]
plot_data2[, region_f := factor(gbd_super_region, levels = c("North Africa and Middle East","Sub-Saharan Africa","Asia"))]
relative_gg3 <- ggplot() +
  geom_bar(data=plot_data2[!is.na(total_relative_growth_contribution_mean) & total_relative_growth_contribution_lower >= 0],
           aes(x=country_f,
               y=total_relative_growth_contribution_mean,
               fill=region_f),
           color='black',
           stat='identity') + 
  # geom_point(data=plot_data2[name != 'Pakistan' & !is.na(total_growth_contribution) & total_growth_contribution >= 0],
  #            aes(x=country_f,
  #                y=(out_rate/1000)*total_pop),
  #            size=3) + 
  geom_errorbar(data=plot_data2[total_relative_growth_contribution_lower >= 0,],
                aes(x=country_f, ymin=total_relative_growth_contribution_lower, ymax=total_relative_growth_contribution_upper), width=.4) +
  theme_minimal() + 
  scale_alpha_discrete(guide=FALSE) + 
  scale_fill_manual(name='Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) +
  labs(y='Percent increase in net out-migrants', x='', title=paste0('Percent increase in net out-migrants attributable to growth, 1990-2010.')) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
print(relative_gg3)
dev.off()
write.csv(plot_data2[, c('country_f','total_relative_growth_contribution_mean','total_relative_growth_contribution_lower','total_relative_growth_contribution_upper')], 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_3_relative_data.csv', row.names=FALSE)

summarize_growth(plot_data2, var_name = 'country_contribution')
plot_data2 <- plot_data2[order(-total_growth_contribution_pop_mean)]
write.csv(plot_data2[, c('country_f','country_contribution')], 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_3_data.csv', row.names=FALSE)
pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Figure_3_', Sys.Date(), '.pdf'), width = 12, height = 12)
region_legend <- gLegend(absolute_gg3)
grid.arrange(
  grobs = list(absolute_gg3 + ggtitle('A) Total net out-migrants attributable to growth') + theme(legend.position="none"), relative_gg3 + ggtitle('B) Percent increase in net out-migrants attributable to growth') + theme(legend.position="none"), region_legend),
  widths = c(5,1),
  layout_matrix = rbind(c(1,3),
                        c(2,3))
)
dev.off()

## MAKE TABLE OF +/- AGGREGATES
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

plot_data2 <- copy(plot_data[year %in% target_period & !(name %in% plot_drops), ])
plot_data2[, total_growth_contribution_pop_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2))]
summarize_growth <- function(dt, var_name) {
  dt[, total_growth_contribution_pop_mean := apply(.SD, 1, mean, na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(dt))]
  dt[, total_growth_contribution_pop_upper := apply(.SD, 1, quantile, c(.975), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(dt))]
  dt[, total_growth_contribution_pop_lower := apply(.SD, 1, quantile, c(.025), na.rm=T), .SDcols=grep("^abs_total_growth_contribution_", names(dt))]
  
  dt[, total_growth_contribution_pop_mean := round(total_growth_contribution_pop_mean/1000)]
  dt[, total_growth_contribution_pop_upper := round(total_growth_contribution_pop_upper/1000)]
  dt[, total_growth_contribution_pop_lower := round(total_growth_contribution_pop_lower/1000)]
  
  dt[, (var_name) := paste0(round(total_growth_contribution_pop_mean), '\n(', round(total_growth_contribution_pop_lower), ' to ',round(total_growth_contribution_pop_upper),')')]
}
positive_gbd <- plot_data2[total_growth_contribution_pop_mean>=0, lapply(.SD, sum, na.rm=TRUE), by=c('gbd_region','gbd_super_region'), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2)) ]
positive_gbd <- summarize_growth(positive_gbd, 'Increases')
positive_gbd <- positive_gbd[, c('gbd_region','gbd_super_region','Increases')]
negative_gbd <- plot_data2[total_growth_contribution_pop_mean<=0, lapply(.SD, sum, na.rm=TRUE), by=c('gbd_region','gbd_super_region'), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2)) ]
negative_gbd <- summarize_growth(negative_gbd, 'Decreases')
negative_gbd <- negative_gbd[, c('gbd_region','gbd_super_region','Decreases')]
negative_gbd <- rbind(negative_gbd, data.table(gbd_region='Central Sub-Saharan Africa',gbd_super_region='Sub-Saharan Africa',Decreases='--'))
gbd_changes <- merge(positive_gbd, negative_gbd)
gbd_changes <- gbd_changes[order(gbd_super_region)]
gbd_changes <- gbd_changes[, c('gbd_region','Increases','Decreases')]
setnames(gbd_changes,'gbd_region','gbd_super_region')
setcolorder(gbd_changes, c('gbd_super_region','Increases','Decreases'))

plot_data2 <- merge(plot_data2, wb_ids, by='name', all.x = TRUE)
positive_wb <- plot_data2[total_growth_contribution_pop_mean>=0, lapply(.SD, sum, na.rm=TRUE), by=c('world_bank','wb_id'), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2)) ]
positive_wb <- summarize_growth(positive_wb, 'Increases')
positive_wb <- positive_wb[, c('world_bank','wb_id','Increases')]
negative_wb <- plot_data2[total_growth_contribution_pop_mean<=0, lapply(.SD, sum, na.rm=TRUE), by=c('world_bank','wb_id'), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2)) ]
negative_wb <- summarize_growth(negative_wb, 'Decreases')
negative_wb <- negative_wb[, c('world_bank','wb_id','Decreases')]
#negative_wb <- rbind(negative_wb, data.table(gbd_region='Central Sub-Saharan Africa',gbd_super_region='Sub-Saharan Africa',Decreases='--'))
wb_changes <- merge(positive_wb, negative_wb)
wb_changes <- wb_changes[order(wb_id)]
setnames(wb_changes, 'world_bank','gbd_super_region')
wb_changes <- wb_changes[, c('gbd_super_region','Increases','Decreases')]

positive_global <- copy(plot_data2)
positive_global[, gbd_super_region := 'All countries']
positive_global <- positive_global[total_growth_contribution_pop_mean>=0, lapply(.SD, sum, na.rm=TRUE), by=c('gbd_super_region'), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2)) ]
positive_global <- summarize_growth(positive_global, 'Increases')
positive_global <- positive_global[, c('gbd_super_region','Increases')]
negative_global <- copy(plot_data2)
negative_global[, gbd_super_region := 'All countries']
negative_global <- negative_global[total_growth_contribution_pop_mean<=0, lapply(.SD, sum, na.rm=TRUE), by=c('gbd_super_region'), .SDcols=grep("^abs_total_growth_contribution_", names(plot_data2)) ]
negative_global <- summarize_growth(negative_global, 'Decreases')
negative_global <- negative_global[, c('gbd_super_region','Decreases')]
global_change <- merge(positive_global, negative_global)
global_change <- global_change[, c('gbd_super_region','Increases','Decreases')]

changes <- rbind(global_change, gbd_changes, wb_changes, fill=TRUE)

setcolorder(changes, c('gbd_super_region','Increases','Decreases'))
saveRDS(changes, 'C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/change_table.RDS')




