#Let's look at some larval smelt data to see
#how we might want to set up a larval smelt sampling design

library(tidyverse)

#Import data set, speecifying date and time columns
#This data is from EDI: https://doi.org/10.6073/pasta/daa635d228a8df63dd14903c051d972e 
SLS <- read_csv("qry_AMC_EDI_SLS_Catch.csv", 
                col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                 Time = col_time(format = "%H:%M:%S"), 
                                 Turbidity = col_double()))

#subset just the longfin smelt data
SLSlf = select(SLS, Year:Turbidity, Longfin_Smelt)

#Filter out just the stations in the South Delta, closest to Clifton Court
SLSlf2 = filter(SLSlf, Station %in% c(914, 915, 918))

#look at mean catch by station and survey number

SLSmean = summarize(group_by(SLSlf2, Station, Survey), 
                    mCPUE = mean(Longfin_Smelt), 
                    sdCPUE = sd(Longfin_Smelt),
                    seCPUE = sdCPUE/length(Longfin_Smelt))
View(SLSmean)

#Plot it
ggplot(SLSmean, aes(x = as.factor(Station), y = mCPUE)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mCPUE - seCPUE, ymax = mCPUE + seCPUE), group = "Survey") +
  facet_wrap(~Survey)
  