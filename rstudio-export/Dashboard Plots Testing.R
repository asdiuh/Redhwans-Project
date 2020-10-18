# install.packages('ggsci')
# install.packages("gridExtra")
# install.packages('ggpubr')
# install.packages("extrafont")
# install.packages("extrafontdb")
# install.packages("stringr")
library(stringr)
library(extrafontdb)
library(extrafont)
library(ggsci)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(ggpubr)

rm(list = ls())
# extrafont::loadfonts()

# font_import()
fonts()
fontfamily = fonts()
load(file = 'historical_data.rdata')

HistoricalDashboard <- function(MAssetClass, CuttingDimension, Font, Month1, Month2) {
    
  plotting_data1 = 
    historical_data %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(Month, AssetClass, !!as.symbol(CuttingDimension)) %>% 
    dplyr::summarise(Exposure = sum(Exposure),
                     ECL = sum(ECL))
  
  plotting_data2 =
    historical_data %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(Month, AssetClass) %>% 
    dplyr::summarise(TotalExposure = sum(Exposure),
                     TotalECL = sum(ECL))
  
  plotting_data = 
    plotting_data1 %>% 
    dplyr::left_join(plotting_data2, by = c('Month' = 'Month', 'AssetClass' = 'AssetClass')) %>% 
    dplyr::mutate(ecl_dist = ECL/TotalECL,
                  exposure_dist = Exposure/TotalExposure) %>% 
    filter(AssetClass == MAssetClass, 
           Month >= Month1,
           Month <= Month2) %>% 
    rename(ecl_level = ECL,
           exposure_level = Exposure)
  
  cycle <- function(x,y) {
    
    if (x == 'ECL') {
      Name = 'ecl'
      Var = 'ECL'
    } else{ 
      Name = 'exposure'
      Var = 'Exposure'
      }
    
    t = ggplot2::scale_y_continuous(name = paste0(Var,' (Millions)'), labels = scales::number_format(accuracy = 1,
                                                                                                     scale = 1e-6, 
                                                                                                     prefix = '£', 
                                                                                                     big.mark = ','))
    
    if (y == '_dist') {
      t = ggplot2::scale_y_continuous(name = paste0(Var,' (Distribution)'), labels = scales::number_format(accuracy = 1,
                                                                                                      scale = 1e2, 
                                                                                                      suffix = '%', 
                                                                                                      big.mark = ','))
      }
    
    temp = 
      plotting_data %>% 
      ggplot2::ggplot()+
      ggplot2::geom_col(aes_string(x = 'Month',y = paste0(Name, y), fill = CuttingDimension))+
      ggplot2::theme(text = element_text(family = Font,face = 'bold'),
                     axis.title = element_text(family = Font,face = 'bold'),
                     axis.text = element_text(family = Font,face = 'bold'),
                     axis.text.x = element_text(family = Font, face = 'bold', angle = 0, hjust = 0.5, vjust = 0.5),
                     axis.line = element_blank(),
                     plot.background = element_blank(),
                     panel.background = element_blank(),
                     panel.border = element_rect(fill = NA))+
      t+
      ggsci::scale_fill_futurama()
    
    assign(paste0('dashboard_', Name, y), temp, envir = .GlobalEnv)

  }
  
  arrange_plots <- function(x) {
    temp = 
      ggpubr::ggarrange(get(paste0('dashboard_ecl',x)) + ggplot2::theme(axis.title.x = element_blank(),
                                                             axis.text.x = element_blank(),
                                                             legend.position = 'none',
                                                             plot.margin = unit(x = c(0,0,0,0),units = 'cm')),
                        get(paste0('dashboard_exposure',x)) + ggplot2::theme(legend.position = 'none'), 
                        ncol = 1,
                        labels = NA,
                        common.legend = TRUE,
                        legend = 'bottom',
                        align = 'v')
    
    assign(paste0('historical_plots',x), 
           ggpubr::annotate_figure(temp, 
                                   top = ggpubr::text_grob(label = paste0('Historical ECL and Exposure'),
                                                           family = Font, 
                                                           face = 'bold')),
           envir = .GlobalEnv)

  }
  
  metric = c('ECL', 'Exposure')
  graph_type = c('_level', '_dist')
  
  lapply(metric, function(x) lapply(graph_type, function(y) cycle(x,y)))  
  lapply(graph_type, function(x) arrange_plots(x))
  
}



historical_data %>% select(Month) %>% distinct(Month) %>% mutate(MonthPost = as.numeric(as.character(Month)) + 6,
                                                                 MonthPrior = as.numeric(as.character(Month)) - 6,
                                                                 MonthPost = case_when(MonthPost%%100 > 12 ~ paste0(MonthPost%/%100+1,'0',(MonthPost%%100 - 12)),              
                                                                 TRUE ~ as.character(MonthPost)))







