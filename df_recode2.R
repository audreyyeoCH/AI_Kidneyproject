# creating four trajectories out of df with four time points

load("doi_10.5061_dryad.h3s0r__v1/df.RData")

# names and types of  trajectories
# high_stable = scores > 100
# low_stable = scores of 20 ?? 10
# descending = time point one is >100, ending is 20??10 and t1>t2>t3>t4
# ascending = time point one is 20??10, ending is >100

# initialisation
df$troup = NA

# ascending t1<t2<t3<t4
for (i in 1:3) { 
      while(df[df$time == i,]$v_GFR0 > 
       df[df$time == i + 1]$v_GFR0 > 
       df[df$time = i +2]$v_GFR0 > 
       df[df$time = i+3]$v_GFR0 
} 
{df$troup = "ascending"} }
# Error: unexpected '}' in "}"

while(
  df[df$time == 1,]$v_GFR0 > 
      df[df$time == 2]$v_GFR0 > 
      df[df$time == 3]$v_GFR0 > 
      df[df$time == 4]$v_GFR0
      {df$troup = "ascending"} }


if (df$v_GFR0 < 90) {   # this works
  df$troup = "ascending"
} else {
  df$troup = NA
}

for (i in 1:nrow(df)) { # this works
 if (df$v_GFR0 < 90) {
 df$troup = "ascending"
 } else {
 df$troup = NA
 }
}

for (i in 1:nrow(df)) { # this does not work
  if (df[df$time == 1,]$v_GFR0[i] > 
      df[df$time == 2]$v_GFR0[i] > 
      df[df$time == 3]$v_GFR0[i] > 
      df[df$time == 4]$v_GFR0)[i] {
    df$troup = "ascending"
  } else {
    df$troup = NA
  }
}

for (i in 1:3) 
if (df$v_GFR0 < 90 && df$v_GFR <0.009) {   # this works
  df$troup = "ascending"
} else {
  df$troup = NA
}




