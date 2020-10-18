# install.packages('ggsci')
# install.packages("gridExtra")
# install.packages('ggpubr')
# install.packages("extrafont")
# install.packages("extrafontdb")
# install.packages("stringr")
rm(list = ls())
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

MovementDashboard <- function(MAssetClass, MPeriod, reporting_difference, Font) {
  
  distinct_months = 
    distinct(data.frame(Month = historical_data$Month)) %>% 
    mutate(MonthPost = as.numeric(as.character(Month))+reporting_difference, 
           MonthPost = case_when(MonthPost%%100 > 12 ~ as.numeric(paste0(MonthPost%/%100 + 1,'0',(MonthPost%%100)%%12)),
                                 TRUE ~ MonthPost),
           MonthPost = as.character(MonthPost),
           MonthPrior1 = as.numeric(as.character(Month))-reporting_difference,
           MonthPrior2 = case_when((MonthPrior1%%100 >= 90 | MonthPrior1%%100 == 0)  ~ as.numeric(paste0(MonthPrior1%/%100 ,12 - ((100 - MonthPrior1%%10)%%10))),           
                                   TRUE ~ MonthPrior1),
           MonthPrior3 = case_when(MonthPrior1%%100 == 0 ~paste0(MonthPrior1%/%100 - 1, MonthPrior2%%100),
                                   MonthPrior2<=1e5 ~ paste0(MonthPrior1%/%100, '0', MonthPrior2%%10),
                                   TRUE ~ as.character(MonthPrior2))) %>% 
    select(Month, MonthPost, MonthPrior3) %>% 
    rename(MonthPrior = MonthPrior3)
  
  Table1 = 
    historical_data %>% 
    left_join(distinct_months,
              by = 'Month') %>% 
    mutate(Period = paste0(Month, ' to ', MonthPost),
           Month = as.character(Month))
  
  Table2 = 
    historical_data %>% 
    left_join(distinct_months,
              by = 'Month') %>% 
    mutate(Period = paste0(MonthPrior, ' to ', Month),
           Month = as.character(Month))
  
  Table3 = 
    full_join(Table1 %>% mutate(Month.x = as.character(Month), indicator.x = 1),
              Table2 %>% mutate(Month.y = as.character(Month), indicator.y = 1),
              by = c('AccountID' = 'AccountID',
                     'MonthPost' = 'Month')) %>% 
    mutate(MovementFamily = case_when(is.na(indicator.x) ~ 'New Accounts',
                                      is.na(indicator.y) ~ 'Closed Accounts',
                                      TRUE ~ 'Stable'),
           MigrationIndicator = case_when(MovementFamily == 'Stable' ~ paste(Stage.x, 'to', Stage.y),
                                          TRUE ~ MovementFamily),
           ECLMovement = coalesce(ECL.y,0) - coalesce(ECL.x,0),
           ExposureMovement = coalesce(Exposure.y,0) - coalesce(Exposure.x,0),
           Period = coalesce(Period.x, Period.y),
           AssetClass = coalesce(AssetClass.x, AssetClass.y))
  
  VariableType = data.frame(VarType = sapply(Table3, class))
  NumericVariables = VariableType$VarType == 'numeric'
  Table3[NumericVariables][is.na(Table3[NumericVariables])] = 0
  
  WaterfallSummary1 = 
    Table3 %>% 
    ungroup() %>% 
    group_by(Period, AssetClass, MovementFamily, MigrationIndicator) %>% 
    summarise(ECL.x = sum(ECL.x),
              ECL.y = sum(ECL.y),
              Exposure.x = sum(Exposure.x),
              Exposure.y = sum(Exposure.y),
              ECLMovement = sum(ECLMovement),
              ExposureMovement = sum(ExposureMovement))
  
  WaterfallSummary2 = 
    Table3 %>% 
    ungroup() %>% 
    group_by(Period, AssetClass) %>% 
    summarise(ECL.x = sum(ECL.x),
              ECL.y = sum(ECL.y),
              Exposure.x = sum(Exposure.x),
              Exposure.y = sum(Exposure.y),
              ECLMovement = sum(ECLMovement),
              ExposureMovement = sum(ExposureMovement)) %>% 
    mutate(MovementFamily = 'Starting Point',
           MigrationIndicator = 'Starting Point')
  
  WaterfallSummary3 = 
    WaterfallSummary2 %>% 
    mutate(MovementFamily = 'Ending Point',
           MigrationIndicator = 'Ending Point')
  
  WaterfallSummary4 = 
    WaterfallSummary2 %>% 
    mutate(MovementFamily = 'Total Migrated Exposure',
           MigrationIndicator = 'Total Migrated Exposure')
  
  MigrationOrder = c("Starting Point",
                     "Closed Accounts",
                     "New Accounts",
                     "Stage 1 to Stage 1",
                     "Stage 2 to Stage 2",
                     "Stage 3 to Stage 3",
                     "Total Migrated Exposure",
                     "Stage 1 to Stage 2",
                     "Stage 2 to Stage 1",
                     "Stage 2 to Stage 3",
                     "Stage 3 to Stage 2",
                     "Stage 1 to Stage 3",
                     "Stage 3 to Stage 1",
                     "Ending Point") 
  
  WaterfallSummary5 = 
    bind_rows(WaterfallSummary1, WaterfallSummary2, WaterfallSummary3, WaterfallSummary4) %>%
    mutate(MigrationIndicator = factor(MigrationIndicator, levels = MigrationOrder, ordered = TRUE)) %>% 
    arrange(Period, MigrationIndicator) %>% 
    ungroup() %>% 
    group_by(Period) %>% 
    mutate(ECLWaterfallMovement = case_when(MigrationIndicator == 'Starting Point' ~ ECL.x,
                                            MigrationIndicator %in% c("Closed Accounts",
                                                                      "New Accounts",
                                                                      'Stage 1 to Stage 1', 
                                                                      'Stage 2 to Stage 2', 
                                                                      'Stage 3 to Stage 3') ~ ECLMovement,
                                            MigrationIndicator == 'Total Migrated Exposure' ~ 0,
                                            MigrationIndicator %in% c("Stage 1 to Stage 2",
                                                                      "Stage 2 to Stage 1",
                                                                      "Stage 2 to Stage 3",
                                                                      "Stage 3 to Stage 2",
                                                                      "Stage 1 to Stage 3",
                                                                      "Stage 3 to Stage 1",
                                                                      "Ending Point") ~ ECL.y),
           
           ECLWaterfallTop = cumsum(ECLWaterfallMovement),
           
           ECLWaterfallTop = case_when(MigrationIndicator == 'Ending Point' ~ 0,
                                       TRUE ~ ECLWaterfallTop),
           
           ECLWaterfallBottom = coalesce(lag(ECLWaterfallTop,1),0),
           
           ExposureWaterfallMovement = case_when(MigrationIndicator == 'Starting Point' ~ Exposure.x,
                                                 MigrationIndicator %in% c("Closed Accounts",
                                                                           "New Accounts",
                                                                           'Stage 1 to Stage 1', 
                                                                           'Stage 2 to Stage 2', 
                                                                           'Stage 3 to Stage 3') ~ ExposureMovement,
                                                 MigrationIndicator == 'Total Migrated Exposure' ~ -Exposure.x,
                                                 MigrationIndicator %in% c("Stage 1 to Stage 2",
                                                                           "Stage 2 to Stage 1",
                                                                           "Stage 2 to Stage 3",
                                                                           "Stage 3 to Stage 2",
                                                                           "Stage 1 to Stage 3",
                                                                           "Stage 3 to Stage 1",
                                                                           "Ending Point") ~ Exposure.y),
           
           ExposureWaterfallTop = cumsum(ExposureWaterfallMovement),
           
           ExposureWaterfallTop = case_when(MigrationIndicator == 'Ending Point' ~ 0,
                                            TRUE ~ ExposureWaterfallTop),
           
           ExposureWaterfallBottom = coalesce(lag(ExposureWaterfallTop,1),0),
           
           ECLWaterfallColour = case_when(ECLWaterfallMovement  < 0 & !(MigrationIndicator %in% c('Starting Point', 'Ending Point', 'Total Migrated Exposure')) ~ 'ECL Decrease',
                                          ECLWaterfallMovement  > 0 & !(MigrationIndicator %in% c('Starting Point', 'Ending Point')) ~ 'ECL Increase',
                                          TRUE ~ 'Net Position'),
           ECLWaterfallColour = factor(ECLWaterfallColour, levels = c('Net Position',
                                                                      'ECL Increase',
                                                                      'ECL Decrease'), ordered = TRUE),
           
           ExposureWaterfallColour = case_when(ExposureWaterfallMovement  < 0 & !(MigrationIndicator %in% c('Starting Point', 'Ending Point')) ~ 'Exposure Decrease',
                                               ExposureWaterfallMovement  > 0 & !(MigrationIndicator %in% c('Starting Point', 'Ending Point')) ~ 'Exposure Increase',
                                               TRUE ~ 'Net Position'),
           ExposureWaterfallColour = factor(ExposureWaterfallColour, levels = c('Net Position',
                                                                                'Exposure Increase',
                                                                                'Exposure Decrease', ordered = TRUE)),
           
           Ordering = 1,
           Ordering = cumsum(Ordering)) %>% 
    filter(Period == MPeriod, AssetClass == MAssetClass)

  WaterfallSummary6 = 
    distinct(data.frame(Period = WaterfallSummary5$Period, join = 1)) %>% 
    full_join(distinct(data.frame(AssetClass = WaterfallSummary5$AssetClass, join = 1)), by = 'join') %>% 
    full_join(data.frame(MigrationIndicator = MigrationOrder, join = 1), by = 'join') %>% 
    left_join(WaterfallSummary5, by = c('Period' = 'Period', 'AssetClass' = 'AssetClass', 'MigrationIndicator' = 'MigrationIndicator')) 
  
  WaterfallSummary6[lapply(WaterfallSummary6, class) == 'numeric'][is.na(WaterfallSummary6[lapply(WaterfallSummary6, class) == 'numeric'])] = 0
  
  names(WaterfallSummary6)[lapply(WaterfallSummary6, class) != 'numeric']
  
  WaterfallSummary6 = 
    WaterfallSummary6 %>% 
    mutate(ECLWaterfallColour = case_when(ECLWaterfallMovement  < 0 & !(MigrationIndicator %in% c('Starting Point', 'Ending Point')) ~ 'ECL Decrease',
                                          ECLWaterfallMovement  > 0 & !(MigrationIndicator %in% c('Starting Point', 'Ending Point')) ~ 'ECL Increase',
                                          TRUE ~ 'Net Position'),
           ECLWaterfallColour = factor(ECLWaterfallColour, levels = c('Net Position',
                                                                      'ECL Increase',
                                                                      'ECL Decrease'), ordered = TRUE),
           
           ExposureWaterfallColour = case_when(ExposureWaterfallMovement  < 0 & !(MigrationIndicator %in% c('Starting Point', 'Ending Point')) ~ 'Exposure Decrease',
                                               ExposureWaterfallMovement  > 0 & !(MigrationIndicator %in% c('Starting Point', 'Ending Point')) ~ 'Exposure Increase',
                                               TRUE ~ 'Net Position'),
           ExposureWaterfallColour = factor(ExposureWaterfallColour, levels = c('Net Position',
                                                                                'Exposure Increase',
                                                                                'Exposure Decrease', ordered = TRUE)),
           Ordering = 1,
           Ordering = cumsum(Ordering))
  
  WaterfallPlotECL = 
    WaterfallSummary6 %>% 
    ggplot2::ggplot()+
    ggplot2::geom_rect(aes(xmin = Ordering - 0.45, 
                  xmax = Ordering + 0.45, 
                  ymin = ECLWaterfallBottom, 
                  ymax = ECLWaterfallTop,
                  fill = ECLWaterfallColour))+
    ggplot2::scale_x_continuous(name = 'Migration', breaks = 1:length(MigrationOrder), labels = stringr::str_wrap(MigrationOrder,10))+
    ggsci::scale_fill_futurama(name = element_blank())+
    ggplot2::theme(text = element_text(family = Font,face = 'bold'),
                   axis.title = element_text(family = Font,face = 'bold'),
                   axis.text = element_text(family = Font,face = 'bold'),
                   axis.text.x = element_text(family = Font, face = 'bold', angle = 0),
                   axis.line = element_blank(),
                   plot.background = element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_rect(fill = NA),
                   plot.title = element_text(hjust = 0.5))+
    ggplot2::scale_y_continuous(name = 'ECL', labels = scales::number_format(accuracy = 1, 
                                                                             prefix = '£', 
                                                                             scale = 1e-6, 
                                                                             big.mark = ','))+
    ggplot2::ggtitle(label = 'ECL Waterfall')
  
  
  
  WaterfallPlotExposure = 
    WaterfallSummary6 %>% 
    ggplot2::ggplot()+
    ggplot2::geom_rect(aes(xmin = Ordering - 0.45, 
                           xmax = Ordering + 0.45, 
                           ymin = ExposureWaterfallBottom, 
                           ymax = ExposureWaterfallTop,
                           fill = ExposureWaterfallColour))+
    ggplot2::scale_x_continuous(name = 'Migration', breaks = 1:length(MigrationOrder), labels = stringr::str_wrap(MigrationOrder,10))+
    ggsci::scale_fill_futurama(name = element_blank())+
    ggplot2::theme(text = element_text(family = Font,face = 'bold'),
                   axis.title = element_text(family = Font,face = 'bold'),
                   axis.text = element_text(family = Font,face = 'bold'),
                   axis.text.x = element_text(family = Font, face = 'bold', angle = 0),
                   axis.line = element_blank(),
                   plot.background = element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_rect(fill = NA),
                   plot.title = element_text(hjust = 0.5))+
    ggplot2::scale_y_continuous(name = 'Exposure', labels = scales::number_format(accuracy = 1,
                                                                                  prefix = '£', 
                                                                                  scale = 1e-6, 
                                                                                  big.mark = ','))+
    ggplot2::ggtitle(label = 'Exposure Waterfall')
  
  waterfall_final = ggpubr::ggarrange(WaterfallPlotECL + theme(axis.text.x = element_blank(), axis.title.x = element_blank(), plot.title = element_blank()),
                                      WaterfallPlotExposure + theme(plot.title = element_blank()), 
                                      ncol = 1, 
                                      labels = NA, 
                                      align = 'v',
                                      common.legend = TRUE, 
                                      legend = 'bottom')
  
  ggpubr::annotate_figure(waterfall_final,top = ggpubr::text_grob(label = 'Waterfall Charts',
                                                                  family = Font,
                                                                  face = 'bold'))
  
}

HistoricalDashboard(MAssetClass = 'MRTG', CuttingDimension = 'Stage', Font = 'Gentium', Month1 = '201904', Month2 = '202001')
MovementDashboard(MAssetClass = 'MRTG', MPeriod = '201912 to 202001', reporting_difference = 1, Font = 'Gentium')





