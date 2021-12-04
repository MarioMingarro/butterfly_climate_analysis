library(readxl)
albarracin_MICRO_results <- read_excel("Results/Excel/Mean_Max_temp_transects_albarracin_MICRO_results.xlsx")
albarracin_CHELSA_results <- read_excel("Results/Excel/Mean_Max_temp_transects_CHELSA_results.xlsx")

albarracin_MICRO_results <- na.omit(albarracin_MICRO_results[3:10])
albarracin_MICRO_results <- left_join(albarracin_MICRO_results, albarracin_CHELSA_results, "CODIGO")
albarracin_MICRO_results <- albarracin_MICRO_results[-10]

kk<- melt(albarracin_MICRO_results)
micro <- na.omit(filter(kk, kk$variable == "mean_1980_1989.x" | kk$variable == "mean_2009_2018.x"))
chelsa <- na.omit(filter(kk, kk$variable == "mean_1980_1989.y" | kk$variable == "mean_2009_2018.y" ))

micro_plot <- ggplot(micro, aes(x= variable, y =value, fill=variable))+
  geom_violin(aes(fill= variable))+
  geom_boxplot(width=0.1, fill = "gray80")+
  labs(y="ºC",
       title="micro")+
  scale_y_continuous("ºC",c(10,15,20,25,30), limits = c(15,30))+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank())

chelsa_plot <- ggplot(chelsa, aes(x= variable, y =value, fill=variable))+
  geom_violin(aes(fill= variable))+
  geom_boxplot(width=0.1, fill = "gray80")+
  labs(y="ºC",
       title="micro")+
  scale_y_continuous("ºC",c(10,15,20,25,30), limits = c(10,30))+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank())


ggarrange(micro_plot, chelsa_plot)

library(PupillometryR)

micro_plot <- ggplot(micro, aes(x= variable, y =value, fill=variable))+
  geom_flat_violin(aes(fill = variable),position = position_nudge(x = .1, y = 0), trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = variable, y = value, colour = variable),position = position_jitter(width = .1), size = 2, shape = 20)+
  geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("CHELSA results")+
  theme(legend.text = element_blank())

chelsa_plot <- ggplot(chelsa, aes(x= variable, y =value, fill=variable)) +
  geom_flat_violin(aes(fill = variable),position = position_nudge(x = .1, y = 0), trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = variable, y = value, colour = variable),position = position_jitter(width = .1), size = 2, shape = 20)+
  geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("CHELSA results")+
  theme(legend.text = element_blank())


ggarrange(micro_plot, chelsa_plot)

          