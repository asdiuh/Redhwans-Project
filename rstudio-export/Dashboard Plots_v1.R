# install.packages('ggsci')
# install.packages("gridExtra")
# install.packages('ggpubr')
# install.packages("extrafont")
# install.packages("extrafontdb")
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

HistoricalDashboard <- function(AssetClass, CuttingDimension, Font, Month1, Month2) {

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
    dplyr::mutate(ecl_pct = ECL/TotalECL,
                  exp_pct = Exposure/TotalExposure) %>% 
    filter(AssetClass == AssetClass, 
           Month >= Month1,
           Month <= Month2)
    
  dashboard1_exposure_level = 
    plotting_data %>% 
    ggplot2::ggplot()+
    ggplot2::geom_col(aes_string(x = 'Month',y = 'Exposure', fill = CuttingDimension))+
    ggplot2::theme(text = element_text(family = Font,face = 'bold'),
                   axis.title = element_text(family = Font,face = 'bold'),
                   axis.text = element_text(family = Font,face = 'bold'),
                   axis.text.x = element_text(family = Font, face = 'bold', angle = 90, hjust = 0.5, vjust = 0.5),
                   axis.line = element_blank(),
                   plot.background = element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_rect(fill = NA))+
    ggplot2::scale_y_continuous(name = 'Exposure (millions)', labels = scales::number_format(accuracy = 1, 
                                                                                             scale = 1e-6, 
                                                                                             prefix = '£', 
                                                                                             big.mark = ','))+
    ggsci::scale_fill_futurama()
  
  dashboard1_ecl_level = 
    plotting_data %>% 
    ggplot2::ggplot()+
    ggplot2::geom_col(aes_string(x = 'Month',y = 'ECL', fill = CuttingDimension))+
    ggplot2::theme(text = element_text(family = Font,face = 'bold'),
                   axis.title = element_text(family = Font,face = 'bold'),
                   axis.text = element_text(family = Font,face = 'bold'),
                   axis.text.x = element_text(family = Font, face = 'bold', angle = 90, hjust = 0.5, vjust = 0.5),
                   axis.line = element_blank(),
                   plot.background = element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_rect(fill = NA))+
    ggplot2::scale_y_continuous(name = 'ECL (millions)', labels = scales::number_format(accuracy = 0.01, 
                                                                                         scale = 1e-6, 
                                                                                         prefix = '£', 
                                                                                         big.mark = ','))+
    ggsci::scale_fill_futurama()
  
  # dashboard1_exposure_dist = 
  #   plotting_data_distribution %>% 
  #   ggplot2::ggplot()+
  #   ggplot2::geom_col(aes(x = Month,y = Exposure, fill = Stage))+
  #   ggplot2::theme(text = element_text(family = Font,face = 'bold'),
  #                  axis.title = element_text(family = Font,face = 'bold'),
  #                  axis.text = element_text(family = Font,face = 'bold'),
  #                  axis.text.x = element_text(family = Font, face = 'bold', angle = 90, hjust = 0.5, vjust = 0.5),
  #                  axis.line = element_blank(),
  #                  plot.background = element_blank(),
  #                  panel.background = element_blank(),
  #                  panel.border = element_rect(fill = NA))+
  #   ggplot2::scale_y_continuous(name = 'Exposure (millions)', labels = scales::number_format(accuracy = 1, 
  #                                                                                            scale = 1e-6, 
  #                                                                                            prefix = '£', 
  #                                                                                            big.mark = ','))+
  #   ggsci::scale_fill_futurama()
  # 
  # dashboard1_ecl_dist = 
  #   plotting_data_distribution %>% 
  #   ggplot2::ggplot()+
  #   ggplot2::geom_col(aes(x = Month,y = ECL, fill = Stage))+
  #   ggplot2::theme(text = element_text(family = Font,face = 'bold'),
  #                  axis.title = element_text(family = Font,face = 'bold'),
  #                  axis.text = element_text(family = Font,face = 'bold'),
  #                  axis.text.x = element_text(family = Font, face = 'bold', angle = 90, hjust = 0.5, vjust = 0.5),
  #                  axis.line = element_blank(),
  #                  plot.background = element_blank(),
  #                  panel.background = element_blank(),
  #                  panel.border = element_rect(fill = NA))+
  #   ggplot2::scale_y_continuous(name = 'ECL (millions)', labels = scales::number_format(accuracy = 0.01, 
  #                                                                                       scale = 1e-6, 
  #                                                                                       prefix = '£', 
  #                                                                                       big.mark = ','))+
  #   ggsci::scale_fill_futurama()  
  
  historical_plots_level = 
    ggpubr::ggarrange(dashboard1_exposure_level + ggplot2::theme(axis.title.x = element_blank(),
                                                                 axis.text.x = element_blank(),
                                                                 legend.position = 'none',
                                                                 plot.margin = unit(x = c(0,0,0,0),units = 'cm')),
                      dashboard1_ecl_level + ggplot2::theme(legend.position = 'none'), 
                      ncol = 1,
                      labels = NA,
                      common.legend = TRUE,
                      legend = 'bottom',
                      align = 'v')
  
  ggpubr::annotate_figure(historical_plots_level, top = ggpubr::text_grob(label = 'Historical ECL and Exposure',family = Font, face = 'bold'))
  
}
  
HistoricalDashboard(AssetClass = 'MRTG', CuttingDimension = 'Stage', Font = 'Gentium', Month1 = '201904', Month2 = '201904')
