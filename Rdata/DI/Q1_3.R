#find probability of (strictly) exceeding 250 groups after 500 tosses
sum <- 0;
ev <- 0.0;
j <- 0;
i <- 0;
val <- 100000;
t <- 250;
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
  prev_val <- '';
  curr_val <-'';
  for(i in 0:500)
  {
    y <- flip(0.4)
    curr_val <- y
    if(prev_val!=curr_val)
    {
      count <- count + 1;
    }
    prev_val=curr_val
  }
  
  if (count>t)
  {
    count_grp <- count_grp +1;
  }
  else
    count_grp <- count_grp +0;
}
prob <- count_grp/val
  