#find probability of (strictly) exceeding 5 groups and (strictly) exceeding 5 heads after 10 tosses
sum <- 0;
ev <- 0.0;
j <- 0;
i <- 0;
val <- 10000;
t <- 5;
ht <- 5;
count_grp <- 0;
prod <- 0;

#random flip generator function
flip<-function(bias)
{
  x=runif(1)
  y='H'
  if(x<bias){y='T'}
  return(y)
}

for(j in 0:val)
{
  count <- 0;
  count_header <-0;
  prev_val <- '';
  curr_val <-'';
  for(i in 0:10)
  {
    y <- flip(0.4)
    curr_val <- y
    if(prev_val!=curr_val)
    {
      count <- count + 1;
    }
    if(curr_val=='H')
    {
      count_header <- count_header+1;
    }
    prev_val=curr_val
  }
  
  if (count>t && count_header>ht)
  {
    count_grp <- count_grp +1;
  }
  else
    count_grp <- count_grp +0;
}
prob <- count_grp/val

