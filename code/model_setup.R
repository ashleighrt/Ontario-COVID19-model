### model setup, initial conditions, and parameters ###

init.infect <- 150*5 #initial infections
# model indices

sindex <- 1:(n.i*n.j)  #these indices help sort out what's saved where
vindex <- seq(from=max(sindex)+1, by=1, length.out=n.i*n.j)
eindex <- seq(from=max(vindex)+1, by=1, length.out=n.i*n.j)
qindex <- seq(from=max(eindex)+1, by=1, length.out=n.i*n.j)
aindex <- seq(from=max(qindex)+1, by=1, length.out=n.i*n.j)
windex <- seq(from=max(aindex)+1, by=1, length.out=n.i*n.j)
bindex <- seq(from=max(windex)+1, by=1, length.out=n.i*n.j)
cindex <- seq(from=max(bindex)+1, by=1, length.out=n.i*n.j)
yindex <- seq(from=max(cindex)+1, by=1, length.out=n.i*n.j)
zindex <- seq(from=max(yindex)+1, by=1, length.out=n.i*n.j)
gindex <- seq(from=max(zindex)+1, by=1, length.out=n.i*n.j)
hindex <- seq(from=max(gindex)+1, by=1, length.out=n.i*n.j)
icuaindex <- seq(from=max(hindex)+1, by=1, length.out=n.i*n.j)
icubindex <- seq(from=max(icuaindex)+1, by=1, length.out=n.i*n.j)
icucindex <- seq(from=max(icubindex)+1, by=1, length.out=n.i*n.j)
rindex <- seq(from=max(icucindex)+1, by=1, length.out=n.i*n.j)
dindex <- seq(from=max(rindex)+1, by=1, length.out=n.i*n.j)
incindex <- seq(from=max(dindex)+1, by=1, length.out=n.i*n.j)
kindex <- seq(from=max(incindex)+1, by=1, length.out=n.i*n.j)
jindex <- seq(from=max(kindex)+1, by=1, length.out=n.i*n.j)



infants.seq <- 1 # 0-4 yo
kids.seq <- 2:3 #5-14 yo (2 age cats)
adults.seq <-  4:10 # 15-49 yo (7 age cats)
elderly.young.seq <- 11:14 #50-69 yo (4 age cats)
elderly.old.seq <- 15:16 # 70+ yo (2 age cats)


school.seq <- 1:5 # for school closures, assume affects those aged 0-25
work.seq <- 6:14 #for work closures, assume affects those aged 25-69
all.seq <- 1:16 # for general social distancing

infants <- get.indices(infants.seq)
kids <- get.indices(kids.seq) 
adults <- get.indices(adults.seq)
elderly.young <- get.indices(elderly.young.seq)
elderly.old <- get.indices(elderly.old.seq)
school <- get.indices(school.seq)
work <- get.indices(work.seq)
all <- get.indices(all.seq)


healthy.index <- seq(from=1, by=1, length.out = n.i)
comorbid.index <- seq(from=max(healthy.index)+1, by=1, length.out= n.i)
init.inf.mild <- c(as.vector(rmultinom(seq(1:32), size = init.infect*0.8, prob=seq(1:32))))
init.inf.severe <- c(as.vector(rmultinom(seq(1:32), size = init.infect*0.2, prob=seq(1:32))))
yinit <- c(
  S = pop.init,
  V = rep(0, n.i*n.j),
  E = rep(0,n.i*n.j),
  Q = rep(0,n.i*n.j),
  A = rep(0,n.i*n.j),
  W = rep(0,n.i*n.j),
  B = init.inf.mild,  #infectious, mild
  C = init.inf.severe,  #infectious, severe
  Y = rep(0, n.i*n.j),
  Z = rep(0, n.i*n.j),
  G = rep(0, n.i*n.j),
  H = rep(0, n.i*n.j),
  ICUA = rep(0, n.i*n.j),
  ICUB = rep(0, n.i*n.j),
  ICUC = rep(0, n.i*n.j),
  R = rep(0, n.i*n.j),
  D = rep(0, n.i*n.j),
  INC= rep(0, n.i*n.j), 
  K = rep(0, n.i*n.j),
  J = rep(0, n.i*n.j)
)


# parameters
model.dur <- 365*2 # model duration
le <- 82.3 #life expectancy


band.sizes <- data.frame(size = c(length(infants.seq), length(kids.seq), length(adults.seq), length(elderly.young.seq),length(elderly.old.seq)),
                         age_group = c("infants", "kids", "adults", "elderly.young", "elderly.old"))
dat<- read_excel("data/parameters.xlsx", 
                 sheet = "general")
params.dat <- dat %>%
              left_join(band.sizes, by="age_group")

band.sizes.interv <- data.frame(size = c(length(infants.seq), length(kids.seq), 2, length(adults.seq)-2, length(elderly.young.seq),length(elderly.old.seq)),
                         age_group = c("infants", "kids", "young.adults", "adults", "elderly.young", "elderly.old"))


interv.dat <- read_excel("data/parameters.xlsx", 
                         sheet = "interventions")

params.interv <- interv.dat %>%
                  pivot_longer(col = starts_with("value"),
                               names_prefix="value_", 
                               names_to="intervention") %>%
                  left_join(band.sizes.interv, by="age_group")

params.interv.names <- params.interv %>% 
                        filter(parameter=="intervention_name") %>%
                        mutate(intervention = as.factor(intervention)) %>%
                        select(intervention , value) 

n.interv <- nrow(params.interv.names)

interv.on <- 0 #start with intervention turned off for the dynamic interventions

params <- list(
  R0 = params.dat$value[params.dat$parameter=="R0"], 
  latent.period = params.dat$value[params.dat$parameter=="latent.period"],
  inf.period.presymp = params.dat$value[params.dat$parameter=="inf.period.presymp"],
  inf.period.mild = params.dat$value[params.dat$parameter=="inf.period.mild"],
  inf.period.severe = params.dat$value[params.dat$parameter=="inf.period.severe"],
  p.severe = get.age.params("p.severe", params.dat), 
  p.icu = get.age.params("p.icu", params.dat), 
  hosp.dur = params.dat$value[params.dat$parameter=="hosp.dur"],
  icu.a.dur = params.dat$value[params.dat$parameter=="icu.a.dur"],
  icu.b.dur = params.dat$value[params.dat$parameter=="icu.b.dur"],
  icu.c.dur = params.dat$value[params.dat$parameter=="icu.c.dur"],
  cfr.icu = get.age.params("cfr.icu", params.dat),
  p.quarantine.s = as.numeric(params.dat$value[params.dat$parameter=="p.quarantine.s"]),
  p.quarantine.e =  as.numeric(params.dat$value[params.dat$parameter=="p.quarantine.e"]),
  quarantine.dur = as.numeric(params.dat$value[params.dat$parameter=="quarantine.dur"]),
  rr.isolate = as.numeric(params.dat$value[params.dat$parameter=="rr.isolate"]),
  time.to.detect = -1/(log(1-rep(as.numeric(get.age.params("p.detect", params.dat)),n.j))/as.numeric(params.dat$value[params.dat$parameter=="inf.period.mild"])),#convert prob to rate to average time to detect
  rr.contact = as.numeric(params.dat$value[params.dat$parameter=="rr.contact"])
  
)

