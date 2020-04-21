
# calculate contact matrix (and aging, if using) #
  
  #Set up age categories and aging
  # age cats: 1: 0-4; 2: 5-9; 3: 10-14; 4:15-19; 5:20-24; 6:25-29; 7:30-34; 8:35-39; 9:40-44; 10: 45-49; 11:50-54; 12:55-59; 
  #13: 60-64; 14:65-69; 15: 70-74; 16: 75+
  

  # setting up the aging so that the age group sizes remain constant - need to revisit this later

  ages <- seq(5,80,by=5)*365 #define age groups - here assuming equal widths to simplify 
  da <- rep(diff(c(0,ages)),n.j) * age.rr.by.status # widths of age clsses, adjusted for relative pop sizes
  aging <- diag(-1/da)
  aging[row(aging)-col(aging)==1] <- 1/head(da,-1) ### makes sure rate of entry into next age gp = rate of exit of previous
  aging[is.na(aging)] <- 0
  aging[c(n.i+1), c(n.i, n.i*2)] <- 0 #don't age from oldest age cat to next health status group

  
  births <- matrix(0, nrow=n.i*n.j, ncol=n.i*n.j)
  births[1,16] <- -aging[16, 16]
  births[17, 32] <- -aging[32,32]

  
  ### read in Mossong contact matrix for the United Kingdom and rescale from daily to annual and to represent the modeled population structure
  age.categories = seq(0,75,5)
  m<-contact_matrix(polymod, countries="United Kingdom", age.limits = age.categories, symmetric = T, split=T)

  
  #for the split mixing matrix, normalization * mean.contacts = max eigenvalue
  mixing <- m$matrix
  contacts <- m$contacts
  demog <- pop.dat$pop2019/sum(pop.dat$pop2019)  #fraction of population in each age group -- use Cdn population to calculate beta
  
  xsym <- (m$mean.contacts * m$normalisation) * sweep(sweep(mixing, MARGIN=2, demog, `*`), MARGIN=1, contacts, `*`)   ## this is daily
  
  eig <- max(eigen(xsym)$values)
  
  b <- params[["R0"]]*(1/(params[["inf.period.presymp"]] + 
                weighted.mean(c(params[["inf.period.mild"]], params[["inf.period.severe"]]), 
               c(1-mean(params$p.severe[params$p.severe!=0]), mean(params$p.severe[params$p.severe!=0])))))/eig #probability of transmission given contact with an infectious individual

  #########################################################################
  ### RESHAPE and rescale  xsym to account for multiple groups per age ####
  #########################################################################
  

tmp <- do.call(rbind, replicate(n.j, xsym, simplify=FALSE))
tmp1 <- do.call(cbind, replicate(n.j, tmp, simplify = FALSE))
cm <- sweep(tmp1,MARGIN=2,age.dist.by.status,"*")  

  # add beta and contact matrix to parameter list
  params[["beta"]] <- b
  params[["cm"]] <- cm

