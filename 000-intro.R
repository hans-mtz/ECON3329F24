library(tidyverse)

can<- read.csv("data/final_categories_2014_2023.csv")

## ploting

can_db <-can %>%
    group_by(Year) %>%
    summarise(
        across(
            where(is.numeric),
            mean
        )
    ) %>%
    mutate(
        Country = "OECD",
        ISO_2 = "OECD",
        ISO_3 = "OECD"
    ) %>% rbind(can)

can_db %>%
    filter(
        ISO_3 %in% c("OECD","CAN","USA","EST")
    )%>%
    ggplot(
        aes(x=Year, y=Final.Score, group=factor(ISO_3), fill=factor(ISO_3))
    ) +
   geom_line(aes(color=ISO_3))+
   theme_classic()+
   theme(legend.title = element_blank(), legend.position = "top")+
   ggtitle("Canadian Tax System", sub = "International Tax Competitive Index Ranking")
