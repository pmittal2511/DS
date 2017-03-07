#find probability of (strictly) exceeding 250 groups given we (strictly) exceeded 240 groups after 500 tosses?
sum <- 0;
ev <- 0.0;
j <- 0;
i <- 0;
val <- 100000;
count_grp <- 0;
prob1 <- 0;
prob2 <-0;
final_prob <-0;

#random flip generator function
flip<-function(bias)
{
  x=runif(1)
  y='H'
  if(x<bias){y='T'}
  return(y)
}

#prob. of strictly exceeding 6 groups in 10 tosses
t <- 250;
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
prob1 <- count_grp/val

#prob. of strictly exceeding 240 groups in 500 tosses
t <- 240;
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
prob2 <- count_grp/val
final_prob <- prob1/prob2


