# packages = c("dplyr", "shiny", "ggplot2", "ggsci")
# install.packages(packages)
# install.packages("ggsci")
library(dplyr)
library(shiny)
library(ggplot2)
library(ggsci)

data = read.csv('V3 - ECL.csv') %>% mutate(Month = as.character(Month),
                                           Stage = as.character(Stage),
                                           Exposure = as.numeric(Exposure),
                                           ECL = as.numeric(ECL))
# write.csv(data, 'ifrs9_data.csv')
# View(data)

# View(data %>% ungroup() %>% group_by(Month, Stage) %>% summarise(Exposure = sum(Exposure), 
#                                                                  ECL = sum(ECL)))

dashboard1_exposure = 
  data %>% 
  ggplot2::ggplot()+
  ggplot2::geom_col(aes(x = Month, y = Exposure, fill = Stage))+
  ggplot2::theme(plot.background = element_blank(), 
                 panel.background = element_blank(),
                 panel.border = element_rect(fill = NA), 
                 plot.title = element_text(hjust = 0.5),
                 text = element_text(family = 'calibri',
                                     face = 'bold'),
                 axis.text = element_text(family = 'calibri',
                                          face = 'bold'))+
  ggplot2::scale_y_continuous(name = 'Exposure',
                              labels = scales::number_format(accuracy = 1,
                                                             # scale = 1e-6,
                                                             prefix = '\u00A3',
                                                             big.mark = ','))+
  ggplot2::ggtitle('ECL and Exposure by Reporting Month')+
  ggplot2::scale_fill_manual(values = ggsci::pal_futurama()(nrow(data %>% dplyr::select(Stage) %>% dplyr::distinct())))

dashboard1_ecl = 
  data %>% 
  ggplot2::ggplot()+
  ggplot2::geom_col(aes(x = Month, y = ECL, fill = Stage))+
  ggplot2::theme(plot.background = element_blank(), 
                 panel.background = element_blank(),
                 panel.border = element_rect(fill = NA), 
                 plot.title = element_text(hjust = 0.5),
                 text = element_text(family = 'calibri',
                                     face = 'bold'),
                 axis.text = element_text(family = 'calibri',
                                          face = 'bold'))+
  ggplot2::scale_y_continuous(name = 'ECL',
                              labels = scales::number_format(accuracy = 1,
                                                             # scale = 1e-6,
                                                             prefix = '\u00A3',
                                                             big.mark = ','))+
  ggplot2::ggtitle('ECL and Exposure by Reporting Month')+
  ggplot2::scale_fill_manual(values = ggsci::pal_futurama()(nrow(data %>% dplyr::select(Stage) %>% dplyr::distinct())))

dashboard1_ecl
dashboard1_exposure



# scales::show_col(ggsci::pal_jco()(3))