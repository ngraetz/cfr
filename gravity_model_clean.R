library(data.table)
library(ggplot2)
library(sf)
library(MASS)
library(pscl)

model_data <- readRDS('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/paper_figs/gravity_model_data.RDS')

## REMOVE HIGH INCOME COUNTRIES
wb_ids <- readRDS('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/wb_ids.RDS')
model_data <- merge(model_data, wb_ids, by.x='country_orig', by.y='country', all.x=T)
model_data <- model_data[region_name!='World Bank High Income', ]

polity <- readRDS('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/polity2.RDS')
ssd <- polity[country=='Sudan', ]
ssd[, country := 'South Sudan']
polity <- rbind(polity, ssd)
model_data <- merge(model_data, polity, by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)
model_data <- model_data[!is.na(polity2), ]
model_data <- model_data[!is.na(urbanicity), ]

iv <- 'l.r.15.24'
int_iv <- 'ldi_quantile'
ldi_inc <- 0.33
file_tag <- 'QUANTILES'
use_quants <- TRUE

# outliers <- c('China','Mexico','Saudi Arabia','Kuwait','Pakistan','Iran','India','United Arab Emirates','Malaysia')
outliers <- c('China','India','Pakistan','United Arab Emirates','Equatorial Guinea','Qatar','Kuwait','Bangladesh','Bahrain','Cuba')
outliers <- c('United Arab Emirates','Equatorial Guinea','Qatar','Kuwait','Bahrain','Cuba')
# outliers <- c('Equatorial Guinea','United Arab Emirates','China','India')
# 'China','India','Pakistan','Bangladesh'
# keep_regions <- 'Sub-Saharan Africa'
# outliers <- c('Pakistan','Iran','Mexico','Eritrea','China','United Arab Emirates','Equatorial Guinea','Qatar','Kuwait','Bahrain','Cuba','Saudi Arabia','Jordan','Bangladesh')
outliers <- c('United Arab Emirates','Equatorial Guinea','Qatar','Kuwait','Bahrain','Cuba','Saudi Arabia','China','Pakistan','El Salvador')
outliers <- c('China','India','Cuba','Kuwait','Bahrain','El Salvador','Equatorial Guinea','Yemen','Oman','Saudi Arabia','Libya')
keep_regions <- unique(model_data[, region_f])

## MAKE LDI QUANTILES
quants <- seq(0,0.9,ldi_inc)
model_data[, ldi_quantile := cut(log_ldi_pc, quantile(log_ldi_pc, probs=c(quants,1)), labels=quants, include.lowest = T)]

## RESIDUALIZE MAIN EFFECTS AND CHECK INTERACTION SHAPE
## FIT GRAVITY MODEL
## l.r.15.24*ldi_quantile
# pdf('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/paper_figs_gravity/country_tests.pdf', height=8, width=12)
# gravity_model <- glm(migrants ~ l.r.15.24 + log_ldi_pc + epr_15_24 + gbd_mx_shocks + polity2 + urbanicity + log_ldi_pc_dest_gap + log_distance + log_pop_dest + log_pop_orig + country_orig, data = model_data[!(country_orig %in% outliers) & region_f %in% keep_regions,], family = 'poisson')
# results <- copy(model_data[!(country_orig %in% outliers) & region_f %in% keep_regions, ])
# results[, residual_migrants := log(migrants+1) - predict(gravity_model, results)]
# for(q in as.character(quants)) {
#   print(ggplot(data=results[ldi_quantile==q,], aes(x=l.r.15.24, y=residual_migrants)) + geom_text(aes(label=country_orig)) + geom_smooth(method='lm') + labs(title=q) + theme_bw())
#   test <- lm(residual_migrants ~ l.r.15.24, data = results[ldi_quantile==q & !is.na(residual_migrants), ])
#   print(influencePlot(test))
  # test <- results[, list(residual_migrants=sum(residual_migrants)), by=c('country_orig','year','l.r.15.24','log_ldi_pc')]
  # test[order(-residual_migrants)]
  # print(ggplot(data=test, aes(x=l.r.15.24*log_ldi_pc,y=residual_migrants)) + geom_text(aes(label=country_orig)) + geom_smooth(method='lm'))
# }
# dev.off()

## FULL MODEL
gravity_model <- glm(as.formula(paste0('migrants ~ ',iv,'*',int_iv,' + epr_15_24 + gbd_mx_shocks + polity2 + urbanicity + log_ldi_pc_dest_gap + log_distance + log_pop_dest + log_pop_orig + country_orig')), data = model_data[!(country_orig %in% outliers) & region_f %in% keep_regions,], family = 'poisson')
coef(gravity_model)[!grepl('country_orig|country_dest', names(coef(gravity_model)))]
saveRDS(gravity_model, paste0('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/paper_figs_gravity/model_',file_tag,'.RDS'))

## FIGURE 1: COEFFICIENTS ON GROWTH BY LDI
results <- copy(model_data[!(country_orig %in% outliers) & region_f %in% keep_regions, ])
if(use_quants) {
for(q in quants) {
  results[ldi_quantile==as.character(q),
          pred_coef := ifelse(q!=0,
                              gravity_model$coefficients[paste0(iv,':',int_iv,q)] + gravity_model$coefficients[iv],
                              gravity_model$coefficients[iv])]
}
results[, pred_coef := exp(pred_coef)]
}
if(!use_quants) results[, pred_coef := exp(get(int_iv) * gravity_model$coefficients[paste0(iv,':',int_iv)] + gravity_model$coefficients[iv])]

pdf(paste0('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/paper_figs_gravity/Figure_1_', Sys.Date(), '_',file_tag,'.pdf'), width = 18, height = 10)
ggplot() +
  geom_line(data=unique(results[, c('log_ldi_pc','pred_coef','region_f')]),
            aes(x=log_ldi_pc,
                y=pred_coef),
            color='black',
            size=1) +
  geom_jitter(data=unique(results[, c('log_ldi_pc','pred_coef','region_f')]),
              aes(x=log_ldi_pc,
                  y=pred_coef,
                  fill=region_f),
              shape=21,
              alpha=0.8,
              size=7, width=0.01, height=0.01) +
  geom_hline(yintercept = 1) + 
  geom_vline(xintercept = 0) + 
  lims(x=c(-2.5,1),y=c(1,1.2)) + 
  scale_color_manual(name='', values=c('#253494','#2ca25f','#bdbdbd','#de2d26','#ff7f00','#ffff33'),guide=F) +
  scale_fill_manual(name='Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26','#ff7f00','#ffff33')) +
  guides(fill = guide_legend(override.aes = list(size = 15))) + 
  labs(y='Coefficient of growth on out-migration',x='Income per capita') + 
  theme_bw() + 
  theme(axis.title.y = element_text(size = 20, margin = margin(r=10)),
        axis.title.x = element_text(size = 20, margin = margin(t=10)),
        axis.text = element_text(size = 20),
        legend.key.size = unit(3,'line'),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))
dev.off()

## MAKE PREDICTIONS
create_preds <- function(growth, d) {
  coef_data <- copy(d)
  if(!growth) coef_data[, l.r.15.24 := 0]
  for(c in unique(coef_data[, country_orig])) coef_data[, (paste0('country_orig',c)) := ifelse(country_orig==c,1,0)]
  for(c in unique(coef_data[, country_dest])) coef_data[, (paste0('country_dest',c)) := ifelse(country_dest==c,1,0)]
  for(q in unique(coef_data[, ldi_quantile])) {
    coef_data[, (paste0('ldi_quantile',q)) := ifelse(ldi_quantile==q,1,0)]
    coef_data[, (paste0('l.r.15.24:ldi_quantile',q)) := get(paste0('ldi_quantile',q)) * l.r.15.24]
  }
  coef_data[, ('l.r.15.24:log_ldi_pc') := l.r.15.24 * log_ldi_pc]
  coef_data[, ('(Intercept)') := 1]
  betas <- MASS::mvrnorm(1000, mu = coef(gravity_model), Sigma = vcov(gravity_model))
  new_d <- coef_data[, colnames(betas), with=F]
  setcolorder(new_d, colnames(betas))
  ## 1000 predictions
  preds <- betas %*% t(as.matrix(new_d))
  preds <- as.data.table(t(preds))
  cols <- paste0('draw',1:1000)
  setnames(preds, cols)
  preds[, (cols) := lapply(.SD,exp), .SDcols=cols]
  return(preds)
}
cols <- paste0('draw',1:1000)
preds_growth <- create_preds(growth=T, d=results)
preds_no_growth <- create_preds(growth=F, d=results)
pred_diffs <- preds_growth - preds_no_growth

results <- cbind(results, preds_growth)
setnames(results, cols, paste0('growth_',cols))
results <- cbind(results, preds_no_growth)
setnames(results, cols, paste0('nogrowth_',cols))
results <- cbind(results, pred_diffs)

preds_growth[, pred_mean := apply(.SD,1,mean), .SDcols=cols]
preds_growth[, pred_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
preds_growth[, pred_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]

preds_no_growth[, pred_no_growth_mean := apply(.SD,1,mean), .SDcols=cols]
preds_no_growth[, pred_no_growth_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
preds_no_growth[, pred_no_growth_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]

results <- cbind(results, preds_growth[, c('pred_mean'), with=F])
results <- cbind(results, preds_no_growth[, c('pred_no_growth_mean'), with=F])

## Get means with predict to compare
results[, pred := exp(predict(gravity_model, results))]
results[, (iv) := 0]
results[, pred_no_growth := exp(predict(gravity_model, results))]

## COUNTRY AGGREGATES
results[, diff := pred - pred_no_growth]
results[diff>0, direction := 'Positive contributions']
results[diff<=0, direction := 'Negative contributions']
get_aggregate_table <- function(levels) {
  ## Total observed migrants
  migrant_totals <- results[, list(migrants=sum(migrants)), by=c(levels)]
  ## Expected percent change in net migrants given no growth
  pred_totals <- results[, lapply(.SD,sum), .SDcols=paste0('growth_',cols), by=c(levels)]
  pred_no_growth_totals <- results[, lapply(.SD,sum), .SDcols=paste0('nogrowth_',cols), by=c(levels)]
  perc_cols <- paste0('perc_draw',1:1000)
  these_levels <- pred_totals[, levels, with=F]
  all <- as.matrix(pred_totals[, paste0('growth_',cols), with=F]) / as.matrix(pred_no_growth_totals[, paste0('nogrowth_',cols), with=F])
  all <- as.data.table(all)
  setnames(all, perc_cols)
  all <- cbind(these_levels, all)
  all[, perc_mean := apply(.SD,1,mean), .SDcols=perc_cols]
  all[, perc_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=perc_cols]
  all[, perc_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=perc_cols]
  all <- all[, c(levels,'perc_mean','perc_lower','perc_upper'), with=F]
  ## Total expected net migrants given no growth
  pred_no_growth_totals <- results[, lapply(.SD,sum), .SDcols=cols, by=c(levels)]
  pred_no_growth_totals[, diff_mean := apply(.SD,1,mean), .SDcols=cols]
  pred_no_growth_totals[, diff_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
  pred_no_growth_totals[, diff_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]
  pred_no_growth_totals <- pred_no_growth_totals[, c(levels,'diff_mean','diff_lower','diff_upper'), with=F]
  ## Total positive contribution
  positive_cont <- results[diff>0, list(positive_cont=sum(diff)), by=c(levels)]
  positive_cont <- results[diff>0, lapply(.SD,sum), .SDcols=cols, by=c(levels)]
  positive_cont[, positive_mean := apply(.SD,1,mean), .SDcols=cols]
  positive_cont[, positive_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
  positive_cont[, positive_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]
  positive_cont <- positive_cont[, c(levels,'positive_mean','positive_lower','positive_upper'), with=F]
  ## Total negative contribution
  negative_cont <- results[diff<=0, list(negative_cont=sum(diff)), by=c(levels)]
  negative_cont <- results[diff<=0, lapply(.SD,sum), .SDcols=cols, by=c(levels)]
  negative_cont[, negative_mean := apply(.SD,1,mean), .SDcols=cols]
  negative_cont[, negative_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
  negative_cont[, negative_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]
  negative_cont <- negative_cont[, c(levels,'negative_mean','negative_lower','negative_upper'), with=F]
  ## Combine
  conts <- merge(positive_cont, negative_cont, all.x=T, all.y=T)
  conts[is.na(positive_mean), positive_mean := 0]
  conts[is.na(negative_mean), negative_mean := 0]
  totals <- Reduce(merge, list(migrant_totals, pred_no_growth_totals, conts, all))
  return(totals)
}
country_aggs <- get_aggregate_table(c('country_orig','region_f'))
country_aggs <- country_aggs[country_orig!='Equatorial Guinea',]

pdf(paste0('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/paper_figs_gravity/Figure_2_', Sys.Date(), '_',file_tag,'.pdf'), width = 10, height = 8)
options(scipen=999)
country_aggs <- country_aggs[order(-positive_mean)]
country_aggs[, country_orig := factor(country_orig, levels=rev(country_orig))]
ggplot() + 
  geom_bar(data=country_aggs[1:18,],
           aes(x=country_orig,
               y=positive_mean,
               fill=region_f),
           color='black',
           stat='identity') +
  # geom_errorbar(data=totals[1:18,],
  #               aes(x=country_orig,
  #                   ymin=positive_lower,
  #                   ymax=positive_upper),
  #               color='black') + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size = 15, margin = margin(r=10)),
              axis.title.x = element_text(size = 15, margin = margin(t=10)),
              axis.text = element_text(size = 12),
              legend.key.size = unit(3,'line'),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 12)) + 
  labs(y='Total out-migrants attributable to growth',x='') + 
  scale_fill_manual(name='Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26','#ff7f00','#ffff33'))

country_aggs <- country_aggs[order(-perc_mean)]
country_aggs[, country_orig := factor(country_orig, levels=rev(country_orig))]
ggplot() + 
  geom_bar(data=country_aggs[1:18,],
           aes(x=country_orig,
               y=(perc_mean-1)*100,
               fill=region_f),
           color='black',
           stat='identity') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size = 15, margin = margin(r=10)),
        axis.title.x = element_text(size = 15, margin = margin(t=10)),
        axis.text = element_text(size = 12),
        legend.key.size = unit(3,'line'),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)) + 
  labs(y='Percent increase in total net migrants attributable to growth',x='') + 
  scale_fill_manual(name='Region', values=c('#253494','#2ca25f','#bdbdbd','#de2d26','#ff7f00','#ffff33'))
dev.off()

## REGION AGGREGATES
# wb_ids <- readRDS('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/processed_data/wb_ids.RDS')
# results <- merge(results, wb_ids, by.x='country_orig', by.y='country', all.x=T)
wb_aggs <- get_aggregate_table('region_name')
setnames(wb_aggs, 'region_name', 'level')
region_aggs <- get_aggregate_table('gbd_region')
region_aggs[order(-diff_mean)]
setnames(region_aggs, 'gbd_region', 'level')
results[, global := 'global']
global_aggs <- get_aggregate_table('global')
setnames(global_aggs, 'global', 'level')
all_aggs <- rbindlist(list(global_aggs,wb_aggs,region_aggs),fill=T)
write.csv(all_aggs, paste0('C:/Users/ngraetz/Dropbox/Penn/papers/cfr_migration/paper_figs_gravity/Table_2_', Sys.Date(), '_',file_tag,'.csv'), row.names = FALSE)

## TRY DESTINATION AGGREGATES
dest_aggs <- get_aggregate_table(c('country_dest','region_f'))
dest_aggs <- dest_aggs[order(-diff_mean)]
head(dest_aggs[region_f=='Asia', c('country_dest','region_f','positive_mean')],20)
