# load initial conditions for population structure #


ages <-  c( "0 to 4 years"    ,   "5 to 9 years" ,    
                 "10 to 14 years",     "15 to 19 years" ,    "20 to 24 years"  ,   "25 to 29 years",    
                 "30 to 34 years" ,    "35 to 39 years" ,    "40 to 44 years" ,    "45 to 49 years"  ,   "50 to 54 years" ,   
                 "55 to 59 years" ,    "60 to 64 years"  ,   "65 to 69 years" ,    "70 to 74 years"  ,   "75 years and over") 

age.bands <- data.frame(age_cats = factor(ages, levels=ages), 
                        levels = seq(1:length(ages)))

n.i <- nrow(age.bands) # number of age groups
n.j <- 2  # number of health states ( 1 = no comorbidities; 2 = comorbidities)


dat <- read_csv(here("data", "pop_estimates_prov.csv")) %>%
        filter(GEO==geo) %>%
        select(-GEO)

prov.prop.pop <- read_csv(here("data", "pop_dist_prov.csv")) %>%
                  filter(GEO==geo) %>%
                  pull(prop_pop)


pop.data <- dat %>%
             mutate(age_group = if_else(age_group %in% c("75 to 79 years","80 to 84 years", "85 to 89 years", "90 to 94 years", "95 to 99 years", "100 years and over"), 
                                                         "75 years and over", age_group)) %>%
             group_by(age_group) %>%
             summarize(pop2019 = sum(pop2019))

dist.over.75 <- dat %>%
                mutate(age_group = if_else(age_group %in% c("80 to 84 years", "85 to 89 years", "90 to 94 years", "95 to 99 years", "100 years and over"), 
                                                              "80+ years", age_group)) %>%
                group_by(age_group) %>%
                summarize(pop2019 = sum(pop2019)) %>%
                filter(age_group %in% c("75 to 79 years", "80+ years")) %>%
                pull(pop2019) 
  

### 2016 CCHS data for comorbidities (chronic heart disease, chronic pulmonary disease, diabetes, cerebrovascular disease, malignancy)
### age groups don't all align with the model age groups so need to do some adjusting
### estimates for 2-11 year olds come from Moran et al, 2009)
comorbid.in <- read_excel("data/cchs_prov.xlsx", 
                          sheet = "CCHS by province")
comorb.data <- comorbid.in %>% 
                  select(comorb = all_of(geo.abb), 
                         age_cats = "Age Category") %>%
                  filter(!is.na(comorb), comorb!=0) 
  
comorb.dat <- comorb.data %>%
                right_join(age.bands, by=c("age_cats")) %>%
                mutate(comorb = if_else(age_cats %in% c("0 to 4 years", "5 to 9 years", "10 to 14 years"),  with(comorb.data, comorb[age_cats=="2 to 11 years"]), 
                                   if_else(age_cats=="75 years and over", 
                                      weighted.mean(c(with(comorb.data, comorb[age_cats=="75 to 79 years"]), with(comorb.data, comorb[age_cats=="80+ years"])), dist.over.75), 
                                      if_else(age_cats=="15 to 19 years", mean(c(with(comorb.data, comorb[age_cats=="15 to 17 years"]), with(comorb.data, comorb[age_cats=="18 to 19 years"]))), comorb))))
  
                   
pop.data %>%
  left_join(comorb.dat, by=c("age_group" = "age_cats")) %>%
  mutate(age_group= factor(age_group, levels=age.bands$age_cats), 
         n_comorb = round(pop2019*comorb,0), 
         n_healthy = pop2019 - n_comorb, 
         p_comorb = n_comorb / pop2019, 
         p_healthy = n_healthy / pop2019) %>%
  arrange(age_group) %>%
  mutate(rr_healthy = n_healthy/n_healthy[1], 
         rr_comorb = n_comorb/n_comorb[1]) -> pop.dat

pop.dat.indexed <- pop.dat %>%
                    select(age_group, contains("n_")) %>%
                    gather(key="key", value = pop_size, -age_group) %>%
                    separate(key, into=c("x", "group")) %>%
                    select(-x) %>%
                    mutate(group = factor(group, levels = c("healthy", "comorb", "preg"))) %>%
                    arrange(group, age_group) %>%
                    mutate(index = row_number())

age.dist.by.status <-  pop.dat %>%
                        select(p_healthy, p_comorb) %>%
                        as.matrix() %>%
                        as.vector()

age.rr.by.status <- pop.dat %>%
                      select(rr_healthy, rr_comorb) %>%
                      as.matrix() %>%
                      as.vector()

pop.init <- pop.dat %>%
              select(n_healthy, n_comorb) %>%
              as.matrix() %>%   
              as.vector()



