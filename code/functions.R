#############
# data prep #
#############
get.indices <- function(age.seq){
  tmp1 <- NULL
  for(i in 1:length(age.seq)){
    tmp <- seq(from = age.seq[i], by = n.i, length.out = n.j)
    tmp1 <- c(tmp1, tmp)
  }
  return(sort(tmp1))
}

get.age.params <- function(param.name, dat){
  dat %>%
    filter(parameter==param.name) %>%
    uncount(size) %>%
    pull(value)
}

get.contact.params.interv <- function(param.name, dat){  ### different age grouping for social distancing 
  dat %>%
    filter(parameter==param.name) %>%
    uncount(size) %>%
    pull(value)
}

###############
# model setup #
###############

addRandomWalk <- function(i, sd){
  
  rw <- map(seq_len(i), ~data.frame(exp(sd*rwiener(end=1, frequency=model.dur+10))))
  tmp <- do.call(cbind, rw)
  names(tmp) <- seq(1:i)
  return(tmp)
  
}

intervSetup <- function(interv.delay, interv.duration, model.dur) {
  interv.on <- c(rep(0, length.out=interv.delay*7), 
                 rep(1, length.out = interv.duration*7), 
                 rep(0, length.out = model.dur - interv.delay*7 - interv.duration*7 +10 ))
}

#################
# model outputs #
#################

runModel <- function(params){
  params[["rw"]] <- addRandomWalk(3, sd=.25) #.25
  traj <- data.frame(ode( y=yinit, times=seq(0,model.dur,by=1),func=model.fun,parms=params, method="rk4" ))
  return(traj)
}


simulateModelReps <- function(n, interv) {
  params.interv.dat <- params.interv %>% 
                        filter(intervention==interv )
  params[["p.quarantine.s.interv"]] <- as.numeric(params.interv.dat$value[params.interv.dat$parameter=="p.quarantine.s"])
  params[["p.quarantine.e.interv"]] <-  as.numeric(params.interv.dat$value[params.interv.dat$parameter=="p.quarantine.e"])
  params[["quarantine.dur.interv"]] <- as.numeric(params.interv.dat$value[params.interv.dat$parameter=="quarantine.dur"])
  params[["rr.isolate.interv"]] <- as.numeric(params.interv.dat$value[params.interv.dat$parameter=="rr.isolate"])
  params[["time.to.detect.interv"]] <- -1/(log(1-rep(as.numeric(get.contact.params.interv("p.detect", params.interv.dat)),n.j))/as.numeric(params.dat$value[params.dat$parameter=="inf.period.mild"]))#convert prob to rate to average time to detect
  params[["rr.contact.interv"]] <- rep(as.numeric(get.contact.params.interv("rr.contact", params.interv.dat)),n.j)
  params[["threshold"]] <- threshold  #only used for dyanamic interventions
  params[["interv.on"]] <- interv.status
  params[["interv.fixed"]] <- interv.fixed
  params[["interv.duration"]] <- interv.duration
  sd.group <- params.interv.dat$value[params.interv.dat$parameter=="group_in_social_distancing"]
  sd.seq <- if (sd.group=="work") work else if (sd.group=="school") school else if (sd.group=="all") all else NULL
  
  x <- matrix(data = params$rr.contact.interv,nrow=n.i*n.j,ncol=n.i*n.j, byrow=F)
  x[,sd.seq] <- x[sd.seq,]
  params[["rr.contact.interv"]] <- x

  ## Output parameters as JSON for the web version.
  jsonlite::write_json(list(y=yinit,params=params,name=params.interv.names$value[interv]),paste("web/params_",str_replace(params.interv.names$value[interv]," ","_"),".json",sep=""),pretty=TRUE,digits=20)

  rep <- as.list(1:n)
  names(rep) <- rep
  if (n > 1) {
    progress = "text"
  }
  else {
    progress = "none"
  }
  traj.rep <- plyr::ldply(rep, function(x) {
    traj <- runModel(params)
  }, .progress = progress, .id = "replicate")
  return(traj.rep)
}

intervRuns <- function(n){
  ## Output a list of scenario's (the webpage picks this up)
  jsonlite::write_json(params.interv.names$value,"web/scenarios.json",pretty=TRUE)  

  interv.rep <- as.list(1:n.interv)
  names(interv.rep) <- params.interv.names$intervention
  interv.rep <- plyr::ldply(interv.rep, function(x) {
    interv.traj <- simulateModelReps(n=n, interv= as.numeric(names(interv.rep[x])))
  }, .progress = "text", .id = "intervention_number")
  return(interv.rep)
}

