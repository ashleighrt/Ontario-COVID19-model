
plotFitModel <- function(n.replicates, time.column = "time", replicate.column = "replicate", interv.column = "intervention_number") {
  
  traj <- intervRuns(n=n.replicates)

traj %>%
  select(time, replicate, intervention_number, interv_delay, interv_duration, contains('inc'), contains('H'), contains('D'), 
         contains('ICU'), contains('K'), contains('J'), 
         contains('Q'), contains('W'), contains('Y'), contains('Z'), contains('G')) %>%
  mutate_at(vars(contains("inc")), function(x) x - lag(x)) %>%
  mutate_at(vars(contains("K")), function(x) x - lag(x)) %>%
  mutate_at(vars(contains("J")), function(x) x - lag(x)) %>%
  filter(time!=0) %>% 
  group_by(replicate, intervention_number) %>%
  gather(key="group", value="cases", -c(time, replicate, intervention_number, interv_delay, interv_duration)) %>%
  separate(group, into=c("metric", "index"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>% 
  mutate(metric = if_else(metric=="ICUA", "H", 
                          if_else(metric=="ICUB", "ICU", 
                                  if_else(metric=="ICUC", "H", metric))), 
         metric = if_else(metric %in% c("Q", "W", "Y", "Z", "G"), "Isolation", metric)) %>%  
  mutate(index = strtoi(index)) %>%
  ungroup() %>%
  data.table() -> traj.tmp

setkey(traj.tmp, time, replicate, intervention_number, interv_delay, interv_duration, metric, index )

traj.tmp.1 <- traj.tmp[,list(cases = sum(cases)),
                by= list(time, replicate, intervention_number, interv_delay, interv_duration, metric, index)]
traj.tmp.1 %>% 
  merge(pop.dat.indexed, by="index") %>% 
  filter(pop_size!=0) %>%
  mutate(age_group = if_else(index %in% c(infants, kids), "Under 15 years", "15 years and older"), 
         age_group = factor(age_group, levels = c("Under 15 years", "15 years and older"))) %>%
  data.table() -> traj.tmp.2

setkey(traj.tmp.2,replicate, age_group, intervention_number, interv_delay, interv_duration, time, metric )
traj.tmp.3 <- traj.tmp.2[,list(tot = sum(cases), 
                pop_tot = sum(pop_size)),
               by= list(age_group, replicate, intervention_number, interv_delay, interv_duration, time, metric)]
traj.tmp.3 %>%
  mutate(rate = tot/pop_tot) -> df

df1 <- data.table(df) 
setkey(df1, age_group, time, replicate, intervention_number, interv_delay, interv_duration, metric)
df.traj <- df1[,list(pop = mean(pop_tot, na.rm=TRUE),
              median = quantile(tot, 0.5, na.rm=TRUE), 
              low_50 = quantile(tot, 0.025, na.rm=TRUE),
              low_95 = quantile(tot, 0.025, na.rm=TRUE), 
              up_50 = quantile(tot, 0.75, na.rm=TRUE),
              up_95 =  quantile(tot, 0.975, na.rm=TRUE)), 
              by= list(age_group, time, metric, intervention_number, interv_delay, interv_duration)]

df.1.1 <- df1[,list(pop = sum(pop_tot, na.rm=TRUE),
                       tot = sum(tot)), 
                 by= list(time, metric, replicate, intervention_number, interv_delay, interv_duration)]
df.traj.1 <- df.1.1[,list(pop = mean(pop, na.rm=TRUE),
                          mean = mean(tot, na.rm = TRUE),
                         median = quantile(tot, 0.5, na.rm=TRUE), 
                         low_95 = quantile(tot, 0.025, na.rm=TRUE), 
                         up_95 =  quantile(tot, 0.975, na.rm=TRUE)), 
                   by= list(time, metric, intervention_number, interv_delay, interv_duration)]

df.traj.1 %>%
  filter(metric %in% c("INC", "H", "ICU")) %>%
  pivot_longer(cols=c(mean, median, low_95, up_95)) %>%
  group_by(metric, intervention_number, name) %>%
  filter(value == max(value)) %>% 
  group_by(metric, name) %>%
  mutate(cases_averted = value[intervention_number=="1"] - value, 
         peak_delay = time - time[intervention_number=="1"]) %>%
  ungroup() %>%
  left_join(params.interv.names, by=c("intervention_number"="intervention")) %>% 
  mutate(metric = factor(metric, levels = c("INC", "H", "ICU"),labels= c("Incidence", "Hospitalizations", "ICU"))) %>%
  arrange(metric, intervention_number) -> peak.dat
  


plot.rates(df.traj, measure="INC", ylabel="Incidence", group="age_group", facet="intervention_number") -> pCr
plot.rates(df.traj, measure ="H", ylabel="Hospitalizations", group="age_group", facet="intervention_number")  -> pHr
plot.rates(df.traj, measure ="ICU", ylabel="ICU", group="age_group", facet="intervention_number")  -> pIr
plot.rates(df.traj, measure ="D", ylabel="Cumulative deaths", group="age_group", facet="intervention_number")  -> pDr


  combined <- (pCr + pHr)/(pIr + pDr) & theme(legend.position = "bottom")
  tmpC <- combined + plot_layout(guides = "collect")


  traj %>%
    select(time, replicate, intervention_number, interv_delay, interv_duration,  
           contains('inc'), contains('K'), contains('J'), contains('D')) %>%
    filter(time==max(time)) %>% 
    gather(key="group", value="value", -c(time, replicate, intervention_number, interv_delay, interv_duration)) %>%
    separate(group, into=c("measure", "index"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>% 
    mutate(index = strtoi(index)) %>%
    merge(pop.dat.indexed) %>% 
    filter(!is.na(pop_size)) %>%
    mutate(age_group = if_else(index %in% infants, "0-4", 
                               if_else(index %in% kids, "5-14", 
                                       if_else(index %in% adults, "15-49", 
                                               if_else(index %in% elderly.young, "50-69", "70+")))), 
           age_group = factor(age_group, levels = c("0-4", "5-14", "15-49", "50-69", "70+")))-> tmp
  
  
  
  tmp1 <- data.table(tmp) 
  setkey(tmp1, age_group, replicate, intervention_number, interv_delay, interv_duration, measure)
  tmp1.1 <- tmp1[,list(AR = 100* sum(value)/sum(pop_size), 
                       Total = sum(value)), 
                 by= list(age_group, measure, replicate, intervention_number, interv_delay, interv_duration)]
  tmp1.2 <- tmp1.1[,list(AttackRate_median = round(quantile(AR, 0.5, na.rm=TRUE),0), 
                         AttackRate_LCI = round(quantile(AR, 0.025, na.rm=TRUE),0), 
                         AttackRate_UCI =  round(quantile(AR, 0.975, na.rm=TRUE),0), 
                         TotalCases_median = round(quantile(Total, 0.5, na.rm=TRUE),0), 
                         TotalCases_LCI = round(quantile(Total, 0.025, na.rm=TRUE),0), 
                         TotalCases_UCI =  round(quantile(Total, 0.975, na.rm=TRUE),0)),
                         by= list(age_group, measure, intervention_number, interv_delay, interv_duration)]
  tmp2.1 <- tmp1[,list(AR = 100* sum(value)/sum(pop_size), 
                       Total = sum(value)), 
                 by= list(replicate, measure, intervention_number,  interv_delay, interv_duration)]
  tmp2.2 <- tmp2.1[,list(AttackRate_median = round(quantile(AR, 0.5, na.rm=TRUE),0), 
                         AttackRate_LCI = round(quantile(AR, 0.025, na.rm=TRUE),0), 
                         AttackRate_UCI =  round(quantile(AR, 0.975, na.rm=TRUE),0), 
                         TotalCases_median = round(quantile(Total, 0.5, na.rm=TRUE),0), 
                         TotalCases_LCI = round(quantile(Total, 0.025, na.rm=TRUE),0), 
                         TotalCases_UCI =  round(quantile(Total, 0.975, na.rm=TRUE),0)),
                   by= list( measure, intervention_number,  interv_delay, interv_duration)]
  
tmp1.2 %>%
    group_by(age_group, measure) %>%
    mutate(cases_averted_median = TotalCases_median[intervention_number=="1"] - TotalCases_median, 
           cases_averted_lcl = TotalCases_LCI[intervention_number=="1"] - TotalCases_LCI,
           cases_averted_ucl = TotalCases_UCI[intervention_number=="1"] - TotalCases_UCI) %>% 
    ungroup() %>%
    left_join(params.interv.names, by=c("intervention_number"="intervention")) %>%
    rename(intervention_name = value) %>%
    mutate(measure = factor(measure, levels = c("INC", "K", "J", "D")))  %>%
    arrange(measure, age_group, intervention_number) %>%
    mutate(measure = if_else(measure=="INC", "Incident infections (all severities)", 
                             if_else(measure=="K", "Cases tested for mild/moderate infection", 
                                     if_else(measure=="J", "Cases requiring hospitalization for severe infection", 
                                             "Deaths")))) %>%
    select(intervention_number, intervention_name, everything()) -> ar.dat


tmp2.2 %>%
  group_by(measure) %>%
  mutate(cases_averted_median = TotalCases_median[intervention_number=="1"] - TotalCases_median, 
         cases_averted_lcl = TotalCases_LCI[intervention_number=="1"] - TotalCases_LCI,
         cases_averted_ucl = TotalCases_UCI[intervention_number=="1"] - TotalCases_UCI) %>% 
  ungroup() %>%
  left_join(params.interv.names, by=c("intervention_number"="intervention")) %>%
  rename(intervention_name = value) %>%
  mutate(measure = factor(measure, levels = c("INC", "K", "J", "D")))  %>%
  arrange(measure, intervention_number) %>%
  mutate(measure = if_else(measure=="INC", "Incident infections (all severities)", 
                           if_else(measure=="K", "Cases tested for mild/moderate infection", 
                                   if_else(measure=="J", "Cases requiring hospitalization for severe infection", 
                                           "Deaths")))) %>%
  select(intervention_number, intervention_name, everything())   -> ar.tot.dat
  
 
df.traj %>%
   mutate(median = round(10^3*median/pop,3)) %>%
   mutate(age_group = if_else(age_group=="Under 15 years", "Under 15 years", "15+ years")) %>%
   select(-c(pop, low_50, up_50, low_95, up_95)) %>%
   pivot_wider(names_from = metric, values_from = c(median)) %>%
   left_join(params.interv.names, by=c("intervention_number"="intervention")) %>% 
   arrange(intervention_number, age_group, time) %>%
   select( Day = time,
           Intervention_number = intervention_number, 
           Intervention_name = value, 
           Intervention_start_wk = interv_delay, 
           Intervention_duration_wks = interv_duration,
           "Age_group"= age_group,
           "New_cases" = INC,
           "Number_in_quarantine_isolation" = Isolation,
           "Mild_cases_seeking_care" = J,
           "New_hospitalizations" = K,
           "Cases_in_hospital" = H,
           "Cases_in_ICU" = ICU,
           "Total_deaths" = D) -> daily.outputs.age

daily.outputs.age %>%
  filter(`Age_group`=="Under 15 years") -> daily.outputs.under15
 
daily.outputs.age %>%
  filter(`Age_group`=="15+ years") -> daily.outputs.15plus

df.traj.1 %>%
  mutate(median = round(10^3*median/pop,3),
         mean = round(10^3*mean/pop,3)) %>%
  mutate(age_cat="All") %>%
  select(-c(pop, low_95, up_95)) %>%
  pivot_wider(names_from = metric, values_from = c(mean, median)) %>%
  left_join(params.interv.names, by=c("intervention_number"="intervention")) %>% 
  arrange(intervention_number, time, age_cat) %>%
  select( Day = time,
          Intervention_number = intervention_number,
          Intervention_name = value, 
          Intervention_start_wk = interv_delay, 
          Intervention_duration_wks = interv_duration,
          "Age_group"= age_cat,
          "New_cases_mean" = mean_INC,
          "New_cases_median" = median_INC,
          "Number_in_quarantine_isolation_mean" = mean_Isolation,
          "Number_in_quarantine_isolation_median" = median_Isolation,
          "Mild_cases_seeking_care_mean" = mean_J,
          "Mild_cases_seeking_care_median" = median_J,
          "New_hospitalizations_mean" = mean_K,
          "New_hospitalizations_median" = median_K,
          "Cases_in_hospital_mean" = mean_H,
          "Cases_in_hospital_median" = median_H,
          "Cases_in_ICU_mean" = mean_ICU,
          "Cases_in_ICU_median" = median_ICU,
          "Total_deaths_mean" = mean_D,
          "Total_deaths_median" = median_D)-> daily.outputs.tot

interv.intensity <- if(interv.fixed==FALSE){
      traj %>%
        select(time, replicate, intervention_number, contains('interv')) %>%
        group_by(replicate, intervention_number) %>%
        summarize(time_interv  = sum(interv_on)/n()) %>%
        group_by(intervention_number) %>%
        summarize(time_interv_median = round(quantile(time_interv, 0.5, na.rm=TRUE),3) ,
                  LCI = round(quantile(time_interv, 0.025, na.rm=TRUE),3),
                  UCI = round(quantile(time_interv, 0.975, na.rm=TRUE),3)) %>%
        left_join(params.interv.names, by=c("intervention_number"="intervention")) %>%
        select(intervention_number, intervention_name=value, everything())
} else {
    NULL
  }
  


return(list(incPlot = pCr,
            hospPlot = pHr, 
            icuPlot = pIr, 
            deathPlot = pDr,
            AR=ar.tot.dat,
            ARbyAge = ar.dat, 
            peakData = peak.dat,
            intervIntensity = interv.intensity,
            dailyOutputsAll = daily.outputs.tot,
            dailyOutputsUnder15 = daily.outputs.under15, 
            dailyOutputs15plus = daily.outputs.15plus ))

}
    
