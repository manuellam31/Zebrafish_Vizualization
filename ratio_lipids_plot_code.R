ggplot(data=d, aes(x=Extraction, y=Ratio, colour = factor(Smallest))) +
  geom_point(size = 3)+
  theme_light()+
  facet_wrap(Class ~ ., scales = "free",ncol = 4)+
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size=11),
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        axis.title.y = element_text(size=11),
        strip.text = element_text(size=11, face = "bold"),)+
  scale_color_manual(values=c('#999999','#E69F00'))