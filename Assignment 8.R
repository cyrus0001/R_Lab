

#Question 1 :
  
print(" H0: Given frequencies (no. of accidents per) month in a certain town) are CONSISTENT with the belief that accident conditions were same during the 10 months.")
print("H1: NOT H0")

#Load the freqency 
O <- c(12,8,20,2,14,10,15,6,9,4)

E <- sum(x)/10
print(E)

r <- sum((O-E)^2/E)

print(r)
#Calculate the degree of freedom 
degree_of_fredom <- 9

#According the table the value  of chi-sqaure of  degree of freedom  9 and  a significance level 5%  is 16.92
chi_squared_value <- 16.92

 if (r < chi_squared_value){
     print(" We failed to reject the null hypothesis at the significance level 5%. The accident conditions are UNIFORM (same) over the 10 month period.")
 }else{
     print("We reject the null hypothesis. The accident conditions are  CERTAINLy NOT UNIFORM (same) over the 10 month period.")
 }


#Question 2(i) : 

print("H0:The sample data fits the distribution.")
print("H1: NOT H0")


#Sample size 
sample_size<-1000

#No of equal parts 
k <- 10
W <- 0
#Generate 1000 sample of unifrom(2,3) distribution
x <-c();
for(i in 1 : sample_size){
  x[i] <- runif(1,2,5)
}

#Sort the sample in increasing order
x <- sort(x)
print(x)
max_value = max(x)
min_value = min(x)

print(max_value)
print(min_value)

#increment is to break the x-range into k equal parts..
O=c() ; E=c() ; incre = ( max_value- min_value)/k ;

for ( j in 1:k){
  m1=min_value
  min_value = m1 + incre
  freq = 0
  for( i in 1:sample_size){
    if(x[i] >= m1 && x[i] <= min_value){
      
      freq = freq + 1
    }
    
  }
  O[j]<-freq
  
  #finding expected frequency
  e=sample_size * ( ( (min_value-2) / 3 ) - ( m1-2 )/3 ) 
  
  E[j]<-e
}

print(" Observed frequency is  ")
print(O)

print(" Expected frequency is  ")
print(E)

#finding W
for( p in 1:k){
  W = W + (O[p]-E[p])^2/E[p]
}
print("W equivalent to chi-square(k-1) distribution.")
print(W)
# from chi square table for k-1 = 10 ,0.05 
# w should be < = 16.92

if( W <= 16.92  ) {
  print( " we failed to reject the null hypothesis." )
}else{
  print(" we reject the null hypothesis. ") 
}


# Question 2(ii):-

print("H0:The sample data fits the distribution.")
print("H1: NOT H0")

library(nleqslv)
sample_size<- 1000
r <- 10
W <- 0
kk=numeric();
for(i in 1:sample_size){
  c <- runif(1)
  
  f1 <- function(x){
    theta <- 2.0
    z <- c- (1-((theta+1+theta*x)/(theta+1)*exp(-theta*x)))
    return (z)
  }
  k <- nleqslv(1.5,f1)
  print(k$message)
  kk[i] = k$x
}
x <- sort(kk)
print(x)

max_value = max(x)
min_value = min(x)

print(max_value)
print(min_value)

#increment is to break the x-range into k equal parts..
O=c() ; E=c() ; incre = ( max_value- min_value)/r ;

for ( j in 1:r){
  m1=min_value
  min_value = m1 + incre
  freq = 0
  for( i in 1:sample_size){
    if(x[i] >= m1 && x[i] <= min_value){
      
      freq = freq + 1
    }
    
  }
  O[j]<-freq
  
  #finding expected frequency
  e=sample_size*( (1 - ((theta + 1 + theta * min_value) / (theta + 1)) * exp(-theta * min_value)) -(1 - ((theta + 1 + theta * m1) / (theta + 1)) * exp(-theta * m1 ) ) )
  
  E[j]<-e
}
print(" Observed frequency is  ")
print(O)

print(" Expected frequency is  ")
print(E)

#finding W
for( p in 1:r){
  W = W + (O[p]-E[p])^2/E[p]
}
print("W equivalent to chi-square(k-1) distribution.")
print(W)
# from chi square table for k-1 = 10 ,0.05 
# w should be < = 16.92

if( W <= 16.92  ) {
  print( " we failed to reject the null hypothesis." )
}else{
  print(" we reject the null hypothesis. ") 
}

#Question 3(i)

print("H0:The sample data fits the distribution.")
print("H1: NOT H0")



#Sample size 
sample_size<-1000

#No of equal parts 
k <- 10
W <- 0
#Generate 1000 sample of unifrom(2,3) distribution
x <-c();
for(i in 1 : sample_size){
  x[i] <- rnorm(1,1,3)
}

#Sort the sample in increasing order
x <- sort(x)
print(x)
max_value = max(x)
min_value = min(x)

print(max_value)
print(min_value)

#increment is to break the x-range into k equal parts..
O=c() ; E=c() ; incre = ( max_value- min_value)/k ;

for ( j in 1:k){
  m1=min_value
  min_value = m1 + incre
  freq = 0
  for( i in 1:sample_size){
    if(x[i] >= m1 && x[i] <= min_value){
      
      freq = freq + 1
    }
    
  }
  O[j]<-freq
  
  #finding expected frequency
  e=sample_size * ( pnorm(min_value,1,3)  - pnorm(m1,1,3) ) 
  
  E[j]<-e
}

print(" Observed frequency is  ")
print(O)

print(" Expected frequency is  ")
print(E)

#finding W
for( p in 1:k){
  W = W + (O[p]-E[p])^2/E[p]
}
print("W equivalent to chi-square(k-1) distribution.")
print(W)
# from chi square table for k-1 = 10 ,0.05 
# w should be < = 16.92

if( W <= 16.92  ) {
  print( " we failed to reject the null hypothesis." )
}else{
  print(" we reject the null hypothesis. ") 
}


#Question 3 (ii) 

print("H0:The sample data fits the distribution.")
print("H1: NOT H0")



sample_size <- 1000
alpha <- 1
beta <-3
k <- 10
W <- 0
sample_space <- function (alpha,beta,sample_size){
  x <- c(); 
  for(i in 1:sample_size){
    y <- runif(1)
    x[i] <- (-1/beta)*log(1-y^(1/alpha))
  }
  return (x)
}

x<-sort(sample_space(alpha,beta,sample_size))
print(x)

max_value = max(x)
min_value = min(x)

print(max_value)
print(min_value)

#increment is to break the x-range into k equal parts..
O=c() ; E=c() ; incre = ( max_value- min_value)/k ;

for ( j in 1:k){
  m1=min_value
  min_value = m1 + incre
  freq = 0
  for( i in 1:sample_size){
    if(x[i] >= m1 && x[i] <= min_value){
      
      freq = freq + 1
    }
    
  }
  O[j]<-freq
  
  #finding expected frequency
  e=sample_size * ( ( (1-exp(-beta*min_value))^alpha ) - (1-exp(-beta* m1) )^alpha ) 
  
  E[j]<-e
}

print(" Observed frequency is  ")
print(O)

print(" Expected frequency is  ")
print(E)

#finding W
for( p in 1:k){
  W = W + (O[p]-E[p])^2/E[p]
}
print("W equivalent to chi-square(k-1) distribution.")
print(W)
# from chi square table for k-1 = 10 ,0.05 
# w should be < = 16.92

if( W <= 16.92  ) {
  print( " we failed to reject the null hypothesis." )
}else{
  print(" we reject the null hypothesis. ")
}

#Question 3 (iii)

print("H0:The sample data fits the distribution.")
print("H1: NOT H0")

sample_size <- 1000
alpha <- 1
beta <-3
k <- 10
W <- 0
sample_space <- function (alpha,beta,sample_size){
  x <- c(); 
  for(i in 1:sample_size){
    y <- runif(1)
    x[i] <- (1-(1-y)^(1/beta))^(1/alpha)
  }
  return (x)
}

x<-sort(sample_space(alpha,beta,sample_size))
print(x)

max_value = max(x)
min_value = min(x)

print(max_value)
print(min_value)

#increment is to break the x-range into k equal parts..
O=c() ; E=c() ; incre = ( max_value- min_value)/k ;

for ( j in 1:k){
  m1=min_value
  min_value = m1 + incre
  freq = 0
  for( i in 1:sample_size){
    if(x[i] >= m1 && x[i] <= min_value){
      
      freq = freq + 1
    }
    
  }
  O[j]<-freq
  
  #finding expected frequency
  e=sample_size * ( (1-(1-min_value^alpha)^beta)-(1-(1-m1^alpha)^beta)) 
  
  E[j]<-e
}

print(" Observed frequency is  ")
print(O)

print(" Expected frequency is  ")
print(E)

#finding W
for( p in 1:k){
  W = W + (O[p]-E[p])^2/E[p]
}
print("W equivalent to chi-square(k-1) distribution.")
print(W)
# from chi square table for k-1 = 10 ,0.05 
# w should be < = 16.92

if( W <= 16.92  ) {
  print( " we failed to reject the null hypothesis." )
}else{
  print(" we reject the null hypothesis. ") 
}












