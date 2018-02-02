#################################
#Intro to modeling in R workshop
#Session 2 - 2/1/18
#################################

#Exercise 1 
#a
#Set your parameters
g=0.8
K=100
f=0.3
N0 = 50
t=50

#create a function called mypop
mypop = function(g, K, f, N0, t){

#create a vector to store population sizes and yield
pop = rep(0, t)
yield = rep(0, t)

#Set initial population size and yield
pop[1] = N0
yield[1]=N0*f

#Run loop to calculate our population growth model
for(n in 2:t){
  pop[n] = pop[n-1] + g*pop[n-1]*(1-pop[n-1]/K) - f*pop[n-1]
  yield[n] = pop[n]*f
  #print(pop[n])
  }
#return(cbind(pop, yield))
return(pop)
}
#call our mypop function
res = mypop(g=0.5, K=100, f=0.3, N0=50, t=50)

#plot the resulting population size
plot(res, type="l")

#b
#create a vector of fishing mortalities
f.sim = seq(0, 1, by=0.1)

#create a vector to store our population
pop = matrix(0, t, length(f.sim))

#set initial population size
pop[1,] = N0

#Run the loop through the matrix to calculate population size over time for each fishing mortality
for(n in 2:nrow(x)){
  for(i in 1:ncol(x)){
    pop[n, i] = pop[n-1, i] + g*pop[n-1, i]*(1-pop[n-1, i]/K) - f.sim[i]*pop[n-1, i]
    }
}
#plot our matrix
matplot(pop, type="l")

#Use the mypop function to create the population matrix
for(n in 1:length(f.sim)){
  res = mypop(g=0.5, K=100, f=f.sim[n], N0=10, t=50)
  if(n==1){out=res}else{out = cbind(out, res)}
}

matplot(out, type="l")

###Lets vectorize!
mypop.vectorize = function(g, K, N0, t){
  
  #Create a vector for pop
  pop = rep(0, length(f.sim))
  #Set initial population sizes
  pop[] = N0
  #Create a vector to store our output
  out=pop
  
  #Create a vector to store total yield
  sum.yield = f.sim*pop
  
  #Plot population size for each fishing mortality
  plot(pop~f.sim, type="l", ylim=c(0, 100))
  
  #Run a loop over all time steps
  for(n in 2:t){
    #print(pop)
    #population growth
    surplus = g*pop*(1-pop/K)
    
    #fishing
    fishing = f.sim*pop
    
    #update our population size
    pop = pop + surplus - fishing
    
    #sum of yield
    sum.yield = sum.yield + fishing
    
    #Add lines to the plot
    lines(pop~f.sim)
    
    #Build an output matrix
    out=rbind(out, pop)
    }
  #return(pop)
  return(out)
}

#Test our function
res = mypop.vectorize(g=0.5, K=100, N0=50, t=50)

#plot our output matrix
matplot(res, type="l")

#Exercise 2

#Create our spatial population model
mypop.patches = function(g, K, f1, f2, N0, t, d, L){
  
  #Create a matrix to store pop
  pop = matrix(0, t, L)
  
  #Set initial population sizes
  pop[1,] = N0
  
  #Create a vector to store fishing mortality in each patch
  f.patches = c(rep(f1, L/2), rep(f2, L/2))
  
  #Loop over our population matrix
  for(n in 2:nrow(pop)){
    for(i in 1:ncol(pop)){
      if(i==1){
        pop[n, i] = pop[n-1, i] + g*pop[n-1, i]*(1-pop[n-1, i]/K) - f.patches[i]*pop[n-1, i] -
        d/2*pop[n-1,i] + d/2*pop[n-1, i+1]}
      else{
        if(i==L){
        pop[n, i] = pop[n-1, i] + g*pop[n-1, i]*(1-pop[n-1, i]/K) - f.patches[i]*pop[n-1, i] -
          d/2*pop[n-1,i] + d/2*pop[n-1, i-1]}
        else{
          pop[n, i] = pop[n-1, i] + g*pop[n-1, i]*(1-pop[n-1, i]/K) - f.patches[i]*pop[n-1, i] -
            d*pop[n-1,i] + d/2*pop[n-1, i-1] + d/2*pop[n-1, i+1]
        }
      }
    }
   }
  
  #return(cbind(pop, yield))
  return(pop)
}

#Test if our model works
res = mypop.patches(g=0.5, K=100, f1=0.3, f2=0.6, N0=50, t=50, d=0.2, L=10)
