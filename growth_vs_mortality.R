pdf(paste0('C:/Users/ngraetz/Documents/Penn/papers/cfr_migration/paper_figs/Growth_vs_mortality_', Sys.Date(), '.pdf'), width = 12, height = 8)
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
  gg <- ggplot(data=mxM_change,
               aes(x=mx_change,
                   y=get(plot_var))) +
    geom_point(aes(fill=world_bank,
                   size=pop15_24_2010,
                   shape=gbd_super_region)) + 
    geom_text_repel(data=mxM_change[mx_change<=-27 | pop_change>=125 | r_pop>=0.45, ],
                    aes(label=name),
                    point.padding=0.5) + 
    labs(y=plot_var_name, x='Change in under-5 mortality rate (per thousand)',title=paste0('Change in 15-24 population vs. child mortality (lag ', lag, '), 1990-2015.')) + 
    scale_size_continuous(name='Population 15-24\n(thousands)', breaks=c(100,250,500,1000,2500,5000), range=c(4,15)) + 
    scale_shape_manual(name='GBD Region', values = c(22,21,23,24)) + 
    scale_fill_manual(name='World Bank Income Group', values=c('#253494','#2ca25f','#bdbdbd','#de2d26', '#ff7f00', '#ffff33')) + 
    guides(fill = guide_legend(override.aes = list(size = 10, fill = c('#253494','#2ca25f','#bdbdbd','#de2d26'), shape=21)),
           shape = guide_legend(override.aes = list(size = 5))) +
    scale_x_reverse() + 
    theme_minimal() 
  print(gg)
dev.off() 