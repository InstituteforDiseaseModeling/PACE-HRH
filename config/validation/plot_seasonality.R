plot_check_seasonality <- function(inputFile, custom_dir){
  ##### Check if the seasonality curve setting is valid #####
  ##### (Code provided by Brittany Hagedorn)                  #####
  
  DS <- read_xlsx(inputFile ,sheet="SeasonalityCurves")
  
  #reshape into long format
  DSmelt <- melt(DS,id.vars = c("Month"),variable.name = "CurveName",value.name="RatioValue")
  
  #set color scheme limits
  YellowMax <- 1/12 * 1.5
  YellowMin <- 1/12 * 0.5
  RedMax <- .25
  
  #set color categories for monthly values
  DSmelt$colorcode <- "Okay"
  DSmelt$colorcode[DSmelt$RatioValue>YellowMax | DSmelt$RatioValue<YellowMin] = "Possible error"
  DSmelt$colorcode[DSmelt$RatioValue>RedMax] = "Validation needed"
  
  #Organize months
  monthslist <- c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")
  DSmelt$Month <- factor(DSmelt$Month,ordered=TRUE,levels=monthslist)
  
  g_monthly <- ggplot(DSmelt,aes(x=Month,y=RatioValue,fill=as.factor(colorcode)))+
    geom_bar(stat="identity")+facet_wrap(~CurveName)+
    scale_fill_manual(values=c("grey","goldenrod1","darkred"))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(legend.title = element_blank())+
    xlab("Month")+ylab("Monthly proportion")+
    labs(title="Validation: Monthtly variation in seasonality values")
  ggsave(file.path(custom_dir, "custom_seasonality.png"), g_monthly, width = 160, height = 80, units="mm")
  
  AnnualTotals <- ddply(DSmelt, .(CurveName), summarize, AnnualTotal=sum(RatioValue))
  
  #Set color code categories for annual totals
  AnnualTotals$colorcode <- "Okay"
  AnnualTotals$colorcode[abs(AnnualTotals$AnnualTotal -1.0) > 1e-2] = "Error"
  
  g_total <- ggplot(AnnualTotals,aes(x=CurveName,y=AnnualTotal,fill=colorcode))+
    geom_bar(stat="identity")+
    scale_fill_manual(values=c("Error"="darkred","Okay"="darkgreen"))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(legend.title = element_blank())+
    xlab("Seasonality Curve")+ylab("Annual Total")+
    labs(title="Validation: Seasonality curves should sum to 1.0")
  ggsave(file.path(custom_dir, "custom_seasonality_total.png"), g_total, width = 120, height = 80, units="mm")
  
}
