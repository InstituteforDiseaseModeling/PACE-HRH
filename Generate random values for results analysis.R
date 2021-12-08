# Generate random results csv so that analysis pipeline can be developed

library(readxl)

dirs = "C:/Users/brittanyha/OneDrive - Bill & Melinda Gates Foundation/Projects/HEP capacity projection/Model"
setwd(dirs)

scenario_list <- c("ScenarioA","ScenarioB")
TrialNum <- seq(1,5)
RunNum <- seq(1,10)
Years <- seq(1,10)
Months <- seq(1,12)

taskvalues <- read_xlsx("R Model Inputs.xlsx",sheet="TaskValues")
taskvalues <- subset(taskvalues,Geography=="National")
TaskList <- taskvalues$Indicator #the first 16 should get me through overhead so have a complex dataset to work with

taskvalues$StartingRateInPop[is.na(taskvalues$StartingRateInPop)] = 0
taskvalues$NumContactsAnnual[is.na(taskvalues$NumContactsAnnual)] = 0
taskvalues$NumContactsPerUnit[is.na(taskvalues$NumContactsPerUnit)] = 0
taskvalues$MinsPerContact[is.na(taskvalues$MinsPerContact)] = 0
taskvalues$FTEratio[is.na(taskvalues$FTEratio)] = 0
taskvalues$HoursPerWeek[is.na(taskvalues$HoursPerWeek)] = 0

YearlyValues <- data.frame(read_xlsx("Output CSV file design.xlsx",sheet="YearlyValues"))
YearlyValues$Lookup <- paste(YearlyValues$Scenario_ID,YearlyValues$Trial_num,YearlyValues$Run_num,YearlyValues$Year,sep="")

RelevantPopSize <- data.frame(poptypes = c("births","1-4","1 yo","2 yo","15 yo girls","adults 18+","1-18","-","all","adults 50+","30 yo adults","18 yo women","women 15-49","50 yo adults"),
                              portion = c(.015,.05,.02,.02,.018,.65,.35,0,1,.25,.02,.02,.2,.015))

#Detailed results outputs sheet = DR

DR <- data.frame()

for(sc in scenario_list){
  for(tr in TrialNum){
    print(paste(sc,"Trial",tr))
    
    for(rn in RunNum){
      for(yr in Years){
        for(mn in Months){

          tdr <- data.frame(Task_ID = TaskList)
          
          tdr$Scenario_ID = sc
          tdr$Trial_num = tr
          tdr$Run_num = rn
          tdr$Year = yr
          tdr$Month = mn
          
          tdr$Lookup = paste(tdr$Scenario_ID,tdr$Trial_num,tdr$Run_num,tdr$Year,sep="")
          tdr$RelevantPop = taskvalues$RelevantPop[1:length(TaskList)]
          tdr$PopProp = RelevantPopSize$portion[match(tdr$RelevantPop,RelevantPopSize$poptypes)]
          tdr$AnnualPop = YearlyValues$Pop_total_start[match(tdr$Lookup,YearlyValues$Lookup)]
          tdr$PopTarget = round(tdr$AnnualPop * tdr$PopProp)
          
          tdr$PortionServed = taskvalues$StartingRateInPop[match(tdr$Task_ID,taskvalues$Indicator)]
          tdr$TimePer = taskvalues$NumContactsPerUnit[match(tdr$Task_ID,taskvalues$Indicator)]*taskvalues$MinsPerContact[match(tdr$Task_ID,taskvalues$Indicator)]/60 + 
                        taskvalues$NumContactsAnnual[match(tdr$Task_ID,taskvalues$Indicator)]*taskvalues$MinsPerContact[match(tdr$Task_ID,taskvalues$Indicator)]/60 
          tdr$AnnualTime = taskvalues$HoursPerWeek[match(tdr$Task_ID,taskvalues$Indicator)]*46
          tdr$FTEratio = taskvalues$FTEratio[match(tdr$Task_ID,taskvalues$Indicator)]

          #Values for the official output file
          tdr$Num_services = tdr$PopTarget * tdr$PortionServed
          tdr$Service_time = tdr$TimePer * tdr$Num_services #clinical services
          tdr$Service_time[tdr$FTEratio>0] = sum(tdr$Service_time[tdr$FTEratio>0]) * tdr$FTEratio[tdr$FTEratio>0] #FTE services, not correct calc
          tdr$Service_time = tdr$Service_time + tdr$AnnualTime #overhead
          tdr$Health_benefit = tdr$Num_services * runif(1)/2
          
          DR = rbind(DR,tdr)
        }
      }
    }
  }
}

write.csv(DR,"Random values for developing analytics.csv")
