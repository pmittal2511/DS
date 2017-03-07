#find expected number of groups after 10 tosses
sum <- 0;
ev <- 0.0;
j <- 0;
i <- 0;
val <- 1000000000

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
  for(i in 0:10)
  {
    y <- flip(0.4)
    curr_val <- y
    if(prev_val!=curr_val)
    {
      count <- count+1;
    }
    prev_val <- curr_val
  }
  sum <- sum+count
}
ev <- sum/val

