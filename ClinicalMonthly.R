for (GeoName in regions){
  #specify path to save PDF to
  destination = paste("results/Clinical monthly","20230112.pdf",sep="_")
  #open PDF
  pdf(file=destination)
  for (GeoName in regions){
    ClinicalMonthly <- Stats_ClinMonth_all %>% 
      filter(Geography_dontedit==GeoName & Scenario_ID=="ComprehensiveModel") 
    plot1 <- ggplot(data=ClinicalMonthly)+
      theme_bw()+
      geom_line(aes(x=Month, y=CI50, group=Year, color = Year),linewidth=1)+
      labs(x=NULL, y="Clinical hours for the month", title = GeoName)
    print(plot1)
  }
  #turn off PDF plotting
  dev.off()
}


ClinicalMonthly <- Stats_ClinMonth_all %>% 
  filter(Geography_dontedit=="Amhara" & Scenario_ID=="ComprehensiveModel") %>%  
  group_by(Year) %>% 
  summarize(MaxMonth = Month[which.max(CI50)], Max_CI50 = max(CI50), MinMonth = Month[which.min(CI50)], Min_CI50 = min(CI50)) %>% 
  mutate(ratio = Max_CI50/Min_CI50)

  
plot1 <- ggplot(data=ClinicalMonthly)+
  theme_bw()+
  geom_line(aes(x=Month, y=CI50, group=Year, color = Year),linewidth=1)+
  labs(x=NULL, y="Clinical hours for the month")
print(plot1)

plot2 <- ggplot(data=ClinicalMonthly)+
  theme_bw()+
  geom_line(aes(x=Year, y=ratio),linewidth=1)+
  ylim(1.02, 1.10)+
  labs(x=NULL, y="Ratio")
print(plot2)

#Max of seasonal monthly clinical hours vs average monthly clinical hours [Figure 5 in manuscript]
Ratio_avgtrials <- ByRun_ClinMonth_all %>% 
  filter(Geography_dontedit == "Oromia" & Scenario_ID=="ComprehensiveModel") %>% 
  group_by(Trial_num, Year) %>% 
  summarize(MeanMonthHrs = mean(TotHrs), MaxMonthHrs = max(TotHrs), MaxMonth = Month[which.max(TotHrs)], MinMonthHrs = min(TotHrs), MinMonth = Month[which.min(TotHrs)])


  
  summarize(MeanMonthHrs = mean(TotHrs), MaxMonthHrs = max(TotHrs), SeasonRatio_Max_Avg = max(TotHrs)/mean(TotHrs)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  summarize(MeanMonthHrs_p05 = quantile(MeanMonthHrs, 0.05), MeanMonthHrs_p25 = quantile(MeanMonthHrs, 0.25),
            MeanMonthHrs_p50 = quantile(MeanMonthHrs, 0.5), MeanMonthHrs_p75 = quantile(MeanMonthHrs, 0.75), 
            MeanMonthHrs_p95 = quantile(MeanMonthHrs, 0.95), MaxToMean_p05 = quantile(SeasonRatio_Max_Avg, 0.05), 
            MaxToMean_p25 = quantile(SeasonRatio_Max_Avg, 0.25), MaxToMean_p50 = quantile(SeasonRatio_Max_Avg, 0.5), 
            MaxToMean_p75 = quantile(SeasonRatio_Max_Avg, 0.75), MaxToMean_p95 = quantile(SeasonRatio_Max_Avg, 0.95)) %>% 
  mutate(MeanMonthHrs_p05_p50 = MeanMonthHrs_p05/MeanMonthHrs_p50, MeanMonthHrs_p95_p50 = MeanMonthHrs_p95/MeanMonthHrs_p50)

check <- ByRun_ClinMonth_all %>% filter(Year==2035 & Geography_dontedit=="Amhara" & Trial_num==1&Scenario_ID=="ComprehensiveModel")

plot_alt <- ggplot(data=ClinHrsSeasonalRatio, aes(x=Year))+
  theme_bw()+
  facet_wrap(~Geography_dontedit)+
  geom_ribbon(aes(ymin = MaxToMean_p05, ymax = MaxToMean_p95), alpha = 0.25)+
  geom_line(aes(x=Year, y=MaxToMean_p50, group=Geography_dontedit),size=1)+
  labs(x=NULL, y="Ratio of seasonal max to seaonal average")
print(plot_alt)

ClinHrsSeasonalRatio_2021 <- ClinHrsSeasonalRatio %>% 
  filter(Year==2021) %>%
  mutate(Region=Geography_dontedit)
plot <- ggplot(data=ClinHrsSeasonalRatio_2021, aes(x=Region))+
  theme_bw()+
  geom_point(aes(y=MaxToMean_p50))+
  geom_errorbar(aes(ymin=MeanMonthHrs_p05_p50, ymax=MeanMonthHrs_p95_p50), width=0.2, position=position_dodge(0.05))+
  guides(x=guide_axis(n.dodge=2))+
  theme(legend.position = "bottom")+
  labs(x=NULL, y="Ratio of seasonal max to seaonal average", title="Seasonality in predicted 2021 clinical workload")
print(plot)

ClinHrsSeasonalRatio_2021_2035 <- ClinHrsSeasonalRatio %>% 
  filter(Year==2021 | Year==2035) %>%
  mutate(Region=Geography_dontedit)
plot <- ggplot(data=ClinHrsSeasonalRatio_2021_2035, aes(x=Region, y=MaxToMean_p50, fill=Region))+
  theme_bw()+
  geom_bar(stat="identity", position="dodge", color = "grey")+
  facet_wrap(vars(Year), ncol=1)+
  guides(x=guide_axis(n.dodge=2))+
  ylim(1, 1.15)+
  theme(legend.position = "bottom")+
  labs(x=NULL, y="Ratio of seasonal max to seaonal average", title="Seasonality in demands for monthly clinical care")
print(plot)

PopulationPlot1 <- ggplot(fertilityrates, aes(x=BandStart, y=InitValue*100, fill=ChangeRateBins))+
  theme_bw()+
  geom_bar(stat="identity", position="dodge", color = "grey")+
  scale_fill_brewer(palette = "RdBu", direction = -1)+
  facet_wrap(vars(adminNameAsUsed))+
  labs(x="Age band starting value", y="Annual fertility per 100 women, 2019", fill="Annual Change Rate")
print(PopulationPlot1)