#####################################
#Intro to modeling in R workshop
#Session 3 - 2/8/2018
#####################################

###Lets vectorize!
mpa.model = function(f, growth, K, N0, t, d, MPA, L){
  
  #Create a vector of fishing mortalities
  #f.patches = c(rep(0, MPA), rep(f, L-MPA))
  
  #MPAs in the middle
  f.patches=rep(f, L)
  if(MPA>0){
    mpa.begin = round((L-MPA)/2) + 1
    mpa.end = mpa.begin + MPA - 1
    f.patches[mpa.begin:mpa.end] = 0
  }
  
  #Create a vector for pop
  pop = rep(0, L)
  total.yield = rep(0, L)
  
  #Set initial population sizes
  pop[] = N0
  
  #Create a vector to store total yield
  total.yield = pop*f.patches
  
  #Run a loop over all time steps
  for(n in 2:t){
    
    #population growth
    surplus = growth*pop*(1-pop/K)
    
    #Emigration
    Emi = c(pop[1]*d/2, pop[2:(L-1)]*d, pop[L]*d/2)
    
    #Immigration
    #Immi = c(pop[2]*d/2, c(pop[1:(L-1)]*d/2 + pop[3:L]*d/2), pop[L-1]*d/2)
    Immi = c(pop[2:L]*d/2, 0) + c(0, pop[1:(L-1)]*d/2)
    
    #fishing
    fishing = f.patches*pop
    
    #update our population size
    pop = pop + surplus - fishing - Emi + Immi
    
    #sum of yield
    total.yield = total.yield + fishing
    
   }
  sum.pop = sum(pop)
  sum.yield.eq = sum(fishing)
  sum.yield = sum(total.yield)
  #return(c(sum.pop, sum.yield.eq, sum.yield, MPA, f, d))
  return(sum.yield.eq)
  }

#Test our function
res = mpa.model(g=0.5, K=100, N0=50, t=50, MPA=50, L = 100, d=0.2, f=0.3)
res

#Optimize
p0 = 0.1
fit = optim(p0, mpa.model, method = c("L-BFGS-B"), lower = 0, upper = 1, control= list(fnscale = -1), 
            growth=0.5, K=100, N0=50, t=50, MPA=50, L = 100, d=0.2)

fit$par

#Calculate optimal fishing mortality for all MPA sizes
mpa.sim = 0:99
for(h in 1:length(mpa.sim)){
  fit = optim(p0, mpa.model, method = c("L-BFGS-B"), lower = 0, upper = 1, control= list(fnscale = -1), 
              growth=0.5, K=100, N0=50, t=50, MPA=mpa.sim[h], L = 100, d=0.2)
  if(h==1){out=c(fit$par, fit$value, mpa.sim[h])}else{out=rbind(out, c(fit$par, fit$value, mpa.sim[h]))}
  print(out)
}
plot(out[,3], out[,1], ylab="optimal fishing mortality", xlab="MPA size")
plot(out[,3], out[,2], ylab="yield", xlab="MPA size")

