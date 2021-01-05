# creating four trajectories out of df with four time points

load("doi_10.5061_dryad.h3s0r__v1/df.RData")

# names and types of  trajectories
# high_stable = scores > 100
# low_stable = scores of 20 ?? 10
# descending = time point one is >100, ending is 20??10 and t1>t2>t3>t4
# ascending = time point one is 20??10, ending is >100

# initialisation
df$troup = NA

# Error: unexpected '}' in "}"
# 
# while(
#   df[df$time == 1,]$v_GFR0 > 
#       df[df$time == 2]$v_GFR0 > 
#       df[df$time == 3]$v_GFR0 > 
#       df[df$time == 4]$v_GFR0
#       {df$troup = "ascending"} }


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

for (i in 1:nrow(df)) { # this does not works
  if (df[df$time == 1 && df$id[i],]$v_GFR0[i] > 
      df[df$time == 2]$v_GFR0[i] > {
    df$troup = "ascending"
  } else {
    df$troup = NA
  }
}


if (df$v_GFR0 < 90 && df$v_GFR <0.009) {   # this works
  df$troup = "ascending"
} else {
  df$troup = NA
}

##### using a smaller df to test code ####
df %>% 
  filter(time == 1:2) ->hey


heytable = hey[c(7,8,9,10)]
View(heytable)

heytable %>% 
  tabyl(time)


ggplot(heytable) +
  geom_histogram(alpha=0.6, binwidth = 5, aes(x = v_GFR0)) +
  theme(legend.position = "none") +
  geom_vline(aes(xintercept= 90),
             color="pink", linetype="dashed", size=1) +
  theme_gray() + labs(x = "L/min")


if () {
  heytable$troup = "hi group"}
else {
  heytable$troup = "lo group"
 }

# if condition
heytable[heytable[id[i]]$time == 1,]$v_GFR0 > heytable[heytable[id[i]]$time == 1,]$v_GFRO
heytable[heytable$id[i],]$troup = "hi group"












