intradayplot = function(x, data){
  ggplotly(
    data%>%
      filter(source ==x) %>%
      mutate(value2 = ifelse(group == 'distance', value*1000, value)) %>%
      mutate(hours = hour(ymdhm)) %>%
      #mutate(ymdhm = ifelse(group %in% c('distance','steps'), hours, ymdhm)) %>%
      # mutate(`AvgValue` = mean(value)) %>% 
      group_by(group, hours) %>%
      mutate(AvgValue = mean(value2)) %>%
      #ungroup() %>%
      ggplot(aes(x = hours, y = AvgValue, group = group, color = group)) +
      geom_point(alpha = 0.5, size =0.7, color = 'slateblue1') +
      geom_smooth(formula = 'y ~ x', 
                  method  = 'loess', 
                  span = 0.1, 
                  se = F, 
                  size = 0.5)+
      theme_minimal() +
      #theme_dark() +
      ylab("값") +
      scale_x_continuous(breaks = c(4, 8, 12, 16, 20, 24), 
                         label = c("4", "8","12", "16", "20", "24"))+
      xlab("시간") +
      facet_wrap(. ~ group, scales = "free")
  )
}


