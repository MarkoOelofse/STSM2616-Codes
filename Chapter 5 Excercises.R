set.seed(3141)

##Question 4

function1=function(x) {
  y=x^2
  return(y)
}

a=5
b=9
n=100

samples=runif(n,min=a,max=b)

y_values=function1(samples)
  
sum=sum(y_values)
  
answer=(1/(b-a))*sum*(1/n)
answer

##Question 5

function1=function(x) {
  y=(sin(x))^2
  return(y)
}

a=0.5
b=1
n=10000

samples=runif(n,min=a,max=b)

y_values=function1(samples)

sum=sum(y_values)

answer=(1/(b-a))*sum*(1/n)
answer

