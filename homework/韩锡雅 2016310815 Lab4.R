#函数mysummary()

mySummary = function(x)
{
  x = as.numeric(x)

  if(is.numeric(x))
  {
   lst1 = list(mean = mean(x),
           var = var(x),
           sd  = sd(x),
           max = max(x),
           min = min(x),
           range = max(x) - min(x),
           med = median(x),
           length = length(x),
           type = typeof(x),
           class = class(x))
   out = unlist(lst1)
   return(out)
  }
}
a = matrix(1:12,3)
a
mySummary(a)

a = round(rnorm(100,0,1),2)
a
mySummary(a)



#解一元二次方程函数:quaSolve()

quaSolve = function(a,b,c) 
{ 
  a = as.numeric(a)
  b = as.numeric(b)
  c = as.numeric(c)
  delta = b^2-4*a*c
  if(delta > 0)
  {
    print("This equation has two distinct real roots:")
    lst = list(x1 = (-b-sqrt(delta))/(2*a),
               x2 = (-b+sqrt(delta))/(2*a))
    return(unlist(lst))
  }

  if(delta == 0)
  {
    print("This equation has two equal real roots:")
    lst = list(x1 = (-b)/(2*a), 
               x2 = (-b)/(2*a))
    return(unlist(lst))
  }
  else
   {
     print("This equation has only complex roots:")
     lst = list(x1 = (-b-sqrt(as.complex(delta)))/(2*a),
                x2 = (-b+sqrt(as.complex(delta)))/(2*a))
     return(unlist(lst))
     }
}


quaSolve(1,"4",1)
quaSolve(1,2,1)
quaSolve(2,1,1)

quaSolve1 = function(a,b,c)
{
  a = as.numeric(a)
  b = as.numeric(b)
  c = as.numeric(c)
  delta1 = b^2-4*a*c
  
  lst = list(x1 = (-b-sqrt(as.complex(delta1)))/(2*a),
               x2 = (-b+sqrt(as.complex(delta1)))/(2*a))
    
  out = unlist(lst)
  return(out)
}

a = c(1,1,2,3,2)
b = c(4,2,1,1,4)
c = c(1,1,1,1,1)
quaSolve1(a,b,c)