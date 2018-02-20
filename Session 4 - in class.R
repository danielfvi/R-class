##############
#Intro to R workshop
#Session 4
#######################

#Mpa model
mpa.model = function(f, growth, K, N0, t, d, MPA, L, rand, price, r){
  
  #Create a vector of fishing mortalities
  #f.patches = c(rep(0, MPA), rep(f, L-MPA))
  
  if(rand=="yes"){
    f.patches=rep(f, L)
    f.patches[round(runif(MPA, 1, 100))]=0
  }else{
  #MPAs in the middle
  f.patches=rep(f, L)
  if(MPA>0){
    mpa.begin = round((L-MPA)/2) + 1
    mpa.end = mpa.begin + MPA - 1
    f.patches[mpa.begin:mpa.end] = 0
  }
  }
  #Create a vector for pop
  pop = rep(0, L)
  total.yield = rep(0, L)
  NPV = 0
  
  #Set initial population sizes
  pop[] = N0
  
  #Create a vector to store total yield
  total.yield = pop*f.patches
  
  #Create an empty plot to plot the revenue trhough time 
  #plot(NA, NA, ylab="Revenue", xlab="time", ylim=c(0, 100000), xlim=c(0, t))
  
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
    
    #Calculate revenue and NPV
    revenue = sum(fishing)*price
    NPV = NPV + revenue*(1/(1+r))^n
    
    #add points to our plot for eery time step
    points(n, revenue)
  }
  #Create a variable for our resulting population size (all patches in year t)
  sum.pop = sum(pop)
  
  #Create a variable for our resulting yield (all patches in year t)
  sum.yield.eq = sum(fishing)
  
  #Create a variable for our resulting yield accross all years and patches
  sum.yield = sum(total.yield)
  
  #Calculate average population size
  EqB = sum(pop)/L
  
  #return function with main results
  return(c(sum.pop, sum.yield.eq, sum.yield, NPV, MPA, f, d))
  
  #return function for calculating optimal fishing mortality
  #return(sum.yield.eq)
  
  #return function for calculation the open access fishing mortality
  #return(abs(EqB-0.2*K))
  }

#Test our function
res = mpa.model(g=0.5, K=100, N0=50, t=50, MPA=30, L = 100, d=0.2, f=0.4, rand="yes", price=50, r=0.05)
res

#Well managed - function needs to return the sum of yield (sum.yield.eq). Here we will maximize the yield by changing 
#the fishing mortality 
p0 = 0.1
fit = optim(p0, mpa.model, method = c("L-BFGS-B"), lower = 0, upper = 1, control= list(fnscale = -1), 
            growth=0.5, K=100, N0=50, t=50, MPA=50, L = 100, d=0.2)

f.wm = fit$par

##Find open access fishing mortality - function needs to return the difference between the average population size and 
#20, which is the population size we expect at open access equilibrium.
p0 = 0.5
fit = optim(p0, mpa.model, method = c("L-BFGS-B"), lower = 0, upper = 1, 
            growth=0.5, K=100, N0=50, t=50, MPA=0, L = 100, d=0.2, rand="no")

f.oa = fit$par

####Simulate MPA effectiveness - simulate the optimal MPA size 100 times with randomly assigned MPA location. 
mpa.sim = 0:99
for(k in 1:100){
for(i in 1:length(mpa.sim)){
  res = mpa.model(g=0.5, K=100, N0=50, t=50, MPA=mpa.sim[i], L = 100, d=0.2, f=f.oa, rand="yes")
  if(i==1){out=res}else{out=rbind(out, res)}
}
  if(k==1){output=out[match(max(out[,3]), out[,3]), 4]}else{output=rbind(output, out[match(max(out[,3]), out[,3]), 4])}  
}

#Create a boxplot of the optimal MPA size for all 100 simulations of MPA design
boxplot(output, ylab="Optimal mpa size")

