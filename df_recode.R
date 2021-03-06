# Contributions by Audrey Yeo and Dr. Diane de Zélicourt
#==================
# load libraries
#==================
library(tidyverse)
library(viridis)
library(ggmosaic)
library(janitor)
library(kml)

#================================================
# preprocessing - to be done only once
#================================================
# load virt pop data
# 384'000 rows x 412 variables, wide form, each row is an individual 
# load("doi_10.5061_dryad.h3s0r__v1/virtppl.1h.RData")
# load("doi_10.5061_dryad.h3s0r__v1/virtppl.1d.RData")
# load("doi_10.5061_dryad.h3s0r__v1/virtppl.1w.RData")
# load("doi_10.5061_dryad.h3s0r__v1/virtppl.1m.RData")
#load("doi_10.5061_dryad.h3s0r__v1/virtppl.ee.RData") # elementary effects

# subsetting data for only interested variables
# inv = c("v_GFR","v_CNA", "v_CKE", "v_HM", "v_PA", "v_HR")
# virtppl.1h$v_CNA # extracellular odium mEq/L
# virtppl.1h$v_CKE # extracellular potassium concentrate
# virtppl.1h$v_HM # haematocrit
# virtppl.1h$v_PA #map
# virtppl.1h$v_HR #heart rate
# virtppl.1hp = virtppl.1h[,inv]
# virtppl.1dp = virtppl.1d[,inv]
# virtppl.1wp = virtppl.1w[,inv]
# virtppl.1mp = virtppl.1m[,inv]

# how many low GFRs are there per time point ?
# all stars of "folklore" by taylor swift
#betty = length(virtppl.1h$v_GFR[virtppl.1h$v_GFR <= 90/1000]) # 2834
#ines = length(virtppl.1d$v_GFR[virtppl.1d$v_GFR <= 90/1000]) # 2771
#james = length(virtppl.1w$v_GFR[virtppl.1w$v_GFR <= 90/1000]) # 2765
#august = length(virtppl.1m$v_GFR[virtppl.1m$v_GFR <= 90/1000]) #2781

# assign ID
# virtppl.1hp$id = c(seq(1,nrow(virtppl.1h),1))
# virtppl.1dp$id = c(seq(1,nrow(virtppl.1h),1))
# virtppl.1wp$id = c(seq(1,nrow(virtppl.1h),1))
# virtppl.1mp$id = c(seq(1,nrow(virtppl.1h),1))
#check
#summary(virtppl.1dp$id)

# assign Time var
# virtppl.1hp$time = data.frame(rep("1", nrow(virtppl.1hp)))
# virtppl.1dp$time = data.frame(rep("2", nrow(virtppl.1dp)))
# virtppl.1wp$time = data.frame(rep("3", nrow(virtppl.1wp)))
# virtppl.1mp$time = data.frame(rep("4", nrow(virtppl.1mp)))


# binarising low and norm GFR #DDZ
virtppl.1hp$group = ifelse(virtppl.1hp$v_GFR < 0.09, "low", "norm")
virtppl.1dp$group = ifelse(virtppl.1dp$v_GFR < 0.09, "low", "norm")
virtppl.1wp$group = ifelse(virtppl.1dp$v_GFR < 0.09, "low", "norm")
virtppl.1mp$group = ifelse(virtppl.1dp$v_GFR < 0.09, "low", "norm")

# global label #DDZ
#   - if it is low at least once (i.e. sum of isLow>=1), then global label is isLowGlobal>=1
#   - if it is never low (i.e. sum of isLow=0), then global label is isLowGlobal=0
virtppl.1hp$isLowGlobal = virtppl.1hp$isLow+virtppl.1dp$isLow+virtppl.1wp$isLow+virtppl.1mp$isLow
virtppl.1wp$isLowGlobal = virtppl.1hp$isLow+virtppl.1dp$isLow+virtppl.1wp$isLow+virtppl.1mp$isLow
virtppl.1dp$isLowGlobal = virtppl.1hp$isLow+virtppl.1dp$isLow+virtppl.1wp$isLow+virtppl.1mp$isLow
virtppl.1mp$isLowGlobal = virtppl.1hp$isLow+virtppl.1dp$isLow+virtppl.1wp$isLow+virtppl.1mp$isLow

virtppl.1hp$group = ifelse(virtppl.1hp$isLowGlobal >0, "low", "norm") ##AY is this the other way
virtppl.1dp$group = ifelse(virtppl.1dp$isLowGlobal >0, "low", "norm")
virtppl.1wp$group = ifelse(virtppl.1wp$isLowGlobal >0, "low", "norm")
virtppl.1mp$group = ifelse(virtppl.1mp$isLowGlobal >0, "low", "norm")

# save(virtppl.1hp, file = "virtppl.1hp.RData") # p for preparation
# save(virtppl.1dp, file = "virtppl.1dp.RData")
# save(virtppl.1wp, file = "virtppl.1wp.RData")
# save(virtppl.1mp, file = "virtppl.1mp.RData")

#==================================================
# PREPROCESSING STEP 2 for creating long form data frame of all time points
# Note that labeling operation done above can be redone here if needed
#==================================================
load("virtppl.1hp.RData")
load("virtppl.1dp.RData")
load("virtppl.1wp.RData")
load("virtppl.1mp.RData")

# aggregate data first by splitting into high and low
# then sampling n out of data frame  (below)
# "q" as postfix in data frame as this step proceeds p
low_1h = virtppl.1hp[virtppl.1hp$group == "low",]
low_1d = virtppl.1dp[virtppl.1dp$group == "low",]
low_1w = virtppl.1wp[virtppl.1wp$group == "low",]
low_1m = virtppl.1mp[virtppl.1mp$group == "low",]

high_1h0 = virtppl.1hp[virtppl.1hp$group == "norm",]
high_1d0 = virtppl.1dp[virtppl.1dp$group == "norm",]
high_1w0 = virtppl.1wp[virtppl.1wp$group == "norm",]
high_1m0 = virtppl.1mp[virtppl.1mp$group == "norm",]

# sample only out of norm group, as only low counts seen in low group
# then sampling n out of data frame (as mentioned above)
set.seed(1964) # Kamala Harris' Birthyear
n = 2800
sampleit = as.numeric(c(sample(1:nrow(virtppl.1hp), n, replace = FALSE)))
# sample only out of norm group, as only low counts seen in low group
high_1h = high_1h0[high_1h0$id %in% sampleit,]
high_1d = high_1d0[high_1d0$id %in% sampleit,]
high_1w = high_1w0[high_1w0$id %in% sampleit,]
high_1m = high_1m0[high_1m0$id %in% sampleit,]

# saving
# save(low_1h, file = "doi_10.5061_dryad.h3s0r__v1/low_1h.RData")
# save(low_1d, file = "doi_10.5061_dryad.h3s0r__v1/low_1d.RData")
# save(low_1w, file = "doi_10.5061_dryad.h3s0r__v1/low_1w.RData")
# save(low_1m, file = "doi_10.5061_dryad.h3s0r__v1/low_1m.RData")
# save
# save(high_1h0, file = "doi_10.5061_dryad.h3s0r__v1/high_1h0.RData")
# save(high_1d0, file = "doi_10.5061_dryad.h3s0r__v1/high_1d0.RData")
# save(high_1w0, file = "doi_10.5061_dryad.h3s0r__v1/high_1w0.RData")
# save(high_1m0, file = "doi_10.5061_dryad.h3s0r__v1/high_1m0.RData")

# save(high_1h, file = "doi_10.5061_dryad.h3s0r__v1/high_1h.RData")
# save(high_1d, file = "doi_10.5061_dryad.h3s0r__v1/high_1d.RData")
# save(high_1w, file = "doi_10.5061_dryad.h3s0r__v1/high_1w.RData")
# save(high_1m, file = "doi_10.5061_dryad.h3s0r__v1/high_1m.RData")

#==================================================
# PREPROCESSING STEP 3
# Note that labeling operation done above can be redone here if needed
#==================================================
load("doi_10.5061_dryad.h3s0r__v1/low_1h.RData")
load("doi_10.5061_dryad.h3s0r__v1/low_1d.RData")
load("doi_10.5061_dryad.h3s0r__v1/low_1w.RData")
load("doi_10.5061_dryad.h3s0r__v1/low_1m.RData")

load("doi_10.5061_dryad.h3s0r__v1/high_1h.RData")
load("doi_10.5061_dryad.h3s0r__v1/high_1d.RData")
load("doi_10.5061_dryad.h3s0r__v1/high_1w.RData")
load("doi_10.5061_dryad.h3s0r__v1/high_1m.RData")


# create all time frame wide form data
ttp = rbind(low_1h, high_1h)
ttq = rbind(low_1d, high_1d)
ttr = rbind(low_1w, high_1w)
tts = rbind(low_1m, high_1m)

ttt = data.frame(rbind(as.matrix(ttp), as.matrix(ttq)))
ttu = data.frame(rbind(as.matrix(ttr), as.matrix(tts)))
df0 = data.frame(rbind(as.matrix(ttt), as.matrix(ttu)))
#save(df0, file = "doi_10.5061_dryad.h3s0r__v1/df0.RData") 
df = data.frame(rbind(as.matrix(ttt), as.matrix(ttu)))


df$v_PA = as.numeric(as.character(df$v_PA))
df$v_GFR = as.numeric(as.character(df$v_GFR))
df$time = as.numeric(as.character(df$time))
df$v_GFR0 <- df$v_GFR*1000
#df$v_PA <- as.numeric(df$v_PA)
#df$time <- as.factor(df$time)
#save(df, file = "doi_10.5061_dryad.h3s0r__v1/df.RData")

load("doi_10.5061_dryad.h3s0r__v1/df.RData")

norm <- df[df$group == "norm",]
low <- df[df$group == "low",]

## plots by groups
ggplot(low) + 
  geom_path(aes(y = v_GFR0, x = time, group = , colour = as.factor(id))) +
  theme(legend.position = "none") + labs(y = "Low, GFR L/min", x = "Time (hours)") 
ggplot(norm) + 
  geom_path(aes(y = v_GFR0, x = time, group = , colour = as.factor(id))) +
  theme(legend.position = "none") + labs(y = "Norm, GFR L/min", x = "Time (hours)") 


#EDA (check)
summary(df$v_PA) # fine
summary(df$v_GFR0) # fine

ggplot(df) +
  geom_histogram(alpha = 0.6, binwidth= 2, aes(x = v_PA)) +
  geom_vline(aes(xintercept= 106.6), color="pink", 
             linetype="dashed", size=1) +
  labs(x = "MAP mmHg")
  

ggplot(df) +
  geom_histogram(alpha=0.6, binwidth = 10, 
                 aes(x = v_GFR0, colour = v_GFR0)) +
  geom_vline(aes(xintercept= 90), color="pink", 
             linetype="dashed", size=1) +
  labs(x = "GFR L/min")

# first longitudinal plot
ggplot(df) +
  geom_path(aes(y = v_PA, x = time, group = , colour = as.factor(id))) +
  theme(legend.position = "none") + labs(y = "MAP mmHG") 

ggplot(df) + 
  geom_path(aes(y = v_GFR0, x = time, group = , colour = as.factor(id))) +
  theme(legend.position = "none") + labs(y = "GFR L/min") 

ggplot(df) +
  geom_path(aes(y = v_GFR0, x = time, group = , colour = as.factor(id))) +
  theme(legend.position = "none")

#ggplot(df) +
#   geom_path(aes(y = v_AU1, x = time, group = , colour = as.factor(id))) +
#   theme(legend.position = "none")
# 
# ggplot(df) +
#   geom_path(aes(y = log(df$p_AARK), x = time, group = , colour = as.factor(id))) +
#   theme(legend.position = "none") 
# 
# ggplot(df) +
#   geom_path(aes(y = log(df$v_GFR), x = time, group = , colour = as.factor(id))) +
#   theme(legend.position = "none") 
  
#==================================================
# KLM CLUSTERING
#==================================================  
######## Cld object needed for kml #####
shortdf <- df[, c(7, 8, 12)]
str(shortdf)
spread(shortdf, time, v_GFR0) -> widedf0
head(widedf0)
sum(is.na(widedf0)) 
#class(widedf0)
#names(widedf0) #is data.frame
widedf1 <- as.matrix(widedf0[, 2:5]) 
#class(widedf1)
#head(widedf1)
#sum(is.nan(widedf1))
#widedf1[is.nan(widedf1)] <- NA
widedf1[is.nan(widedf1)] <- NA
widedf1 <- imputation(widedf1, "trajMean") 
# head(widedf1)
# colnames(widedf1)
sum(is.na(widedf1)) # 0
#class(widedf1)
# create cld for matrix object pour widedf1 which has been imputated
mycld0 <- clusterLongData(widedf1, timeInData = 1:4)
#this likes matrices, the vignette said both
save(mycld0, file = "mycld0")

# nbCluster = 2 
# slow kml not needed
# kml(mycld, toPlot = "both") # runs well, straight from the paper
#choice(mycld)
#plotAllCriterion(mycld) # works # show case
# from now on we use fast kml,
kml(mycld0, nbClusters = 2, parAlgo = parALGO(distance = function(x, y) #fast kml
  +    cor(x, y), saveFreq = 10))

choice(mycld0)

######## nbCluster = 4 #####
#kml(mycld, nbClusters = 4, toPlot = "both")  # slow kml
kml(mycld0, nbClusters = 4, parAlgo = parALGO(distance = function(x, y) #fast kml
  +    cor(x, y), saveFreq = 10))
choice(mycld0)
plotAllCriterion(mycld0) # uneventful

######## df = df_longdf : creating df for nbClusters = 2 clusters #####
str(widedf1)
widedf1 <- as.data.frame(widedf1)
#widedf1$cluster <- widedf0$cluster
widedf1$id <- widedf0$id
#widedf0$Area <- widedf0$Area
widedf1$kmlclusters2 <- getClusters(mycld0, 2, 
                                    asInteger = FALSE) 

######## creating df for nbClusters = 4 clusters #####
widedf1$kmlclusters4 <- getClusters(mycld0, 4, 
                                    asInteger = FALSE) 
# yup it works after we run at least "slow kml", e.g.  kml(mycld, nbRedraw = 2, toPlot = "both")
save(widedf1, file = "widedf0.Rda")




########################################################################
######## Long form df from kml partitions: for glm and lmer models #####
#df_long <- gather(widedf0, "mmyy", 
# "meanPDheel", -EarTag, -Area, -cluster, -kmlclusters2, -kmlclusters4)
gather(widedf1, time, v_GFR0, -id, 
       , -kmlclusters4, -kmlclusters2) -> testlong
testlong$time = as.numeric(testlong$time)
#testlong$time <- factor(testlong$time, 
                       # c("1h", "1d", "1w", "1m"),
                       # ordered = TRUE)
names(testlong)
#df_longdf0 <- full_join(testlong, df, by = "EarTag")
df_longdf <- full_join(df, testlong, by = 
                         c("id" = "id", "time" = "time")) 
class(testlong$time) # numeric
class(df$time) # numeric

View(df_longdf[1:10,1:4])
#colnames(df_longdf)[415] -> "v_GRF0"
save(df_longdf, file = "df_longdf.Rda")

# EDA, important variables for output GFR are:
#df$p_AARK
#df$p_EARK
#df$p_GFLC
#df$p_NID


load("df_longdf.Rda")
df_longA = df_longdf[df_longdf$kmlclusters == "A"]

ggplot(df_longdf) +
  geom_mosaic(aes(x = product(kmlclusters2, kmlclusters4),
                  fill = kmlclusters2)) +
  theme_gray()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8)) +
  ggplot2::labs(x = "kml clusters 4", y = "kml clusters 2")

# v_GFR
ggplot(df_longdf, aes(x = time, y = v_GFR0.y, colour = kmlclusters2, group = kmlclusters2)) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_gray()+
  labs(y = "GFR L/min", x = "time") +
  #geom_vline(xintercept = c(6, ), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.text.x.bottom = element_text(size = 8)) + 
        expand_limits(y=c(110, 120)) 

ggplot(df_longdf, aes(x = as.factor(time), y = v_GFR0.y, colour = kmlclusters4, group = kmlclusters4)) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_gray()+
  labs(y = "GFR mL/min", x = "time", title = "Clusteing into 4 partitians") +
  #geom_vline(xintercept = c(6, ), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x.bottom = element_text(size = 10)) +
  scale_colour_discrete(name = "Four kml\npartitians (n)")

# v_PA
ggplot(df_longdf, aes(as.factor(time), y = v_PA, colour = kmlclusters2, group = kmlclusters2)) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_gray()+
  labs(y = "mmHg", x = "time") +
  #geom_vline(xintercept = c(6, ), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.text.x.bottom = element_text(size = 8)) 
#+
 # expand_limits(y=c(110, 120)) 

ggplot(df_longdf, aes(x = time, y = v_PA, colour = kmlclusters4, group = kmlclusters4)) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_gray()+
  labs(y = "mmHg", x = "time") +
  #geom_vline(xintercept = c(6, ), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.text.x.bottom = element_text(size = 8)) 
#+
 # expand_limits(y=c(110, 117)) 


###
ggplot(df_longdf) + 
  geom_path(aes(y = v_GFR0.y, x = as.factor(time), group = id, colour =  v_GFR0.y)) +
  theme(legend.position = "none") + labs(y = "GFR L/min", x = "time", title = "Clustering into 4 partitians") +
  scale_color_gradientn(colours = c("firebrick", "orange", "navyblue"),
                        values = c(0, 0.5, 1),limits = c(50, 120), oob = scales::squish) +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))


#Zooming in per cluster


df_long4A = df_longdf[df_longdf$kmlclusters4 == "A",]
df_long4B = df_longdf[df_longdf$kmlclusters4 == "B",]
df_long4C = df_longdf[df_longdf$kmlclusters4 == "C",]
df_long4D = df_longdf[df_longdf$kmlclusters4 == "D",]

ggplot(df_long4A) + 
  geom_path(aes(y = v_GFR0.y, x = time, group = id, color = v_GFR0.y), size=0.5, show.legend=FALSE) +
  theme_bw() + labs(y = "GFR L/min", x = "time",title="Partition A")+ ylim(0, 175) + 
  scale_color_gradientn(colours = c("firebrick", "orange", "navyblue"),
                        values = c(0, 0.5, 1),limits = c(50, 120), oob = scales::squish) +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))

ggplot(df_long4B) + 
  geom_path(aes(y = v_GFR0.y, x = time, group = id, color = v_GFR0.y), size=0.5, show.legend=FALSE) +
  theme_bw() + labs(y = "GFR L/min", x = "time",title="Partition B")+ ylim(0, 175) + 
  scale_color_gradientn(colours = c("firebrick", "orange", "navyblue"),
                        values = c(0, 0.5, 1),limits = c(50, 120), oob = scales::squish) +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))

ggplot(df_long4C) + 
  geom_path(aes(y = v_GFR0.y, x = time, group = id, color = v_GFR0.y), size=0.5, show.legend=FALSE) +
  theme_bw() + labs(y = "GFR L/min", x = "time",title="Partition C")+ ylim(0, 175) + 
  scale_color_gradientn(colours = c("firebrick", "orange", "navyblue"),
                        values = c(0, 0.5, 1),limits = c(50, 120), oob = scales::squish)

ggplot(df_long4D) + 
  geom_path(aes(y = v_GFR0.y, x = time, group = id, color = v_GFR0.y), size=0.5, show.legend=FALSE) +
  theme_bw() + labs(y = "GFR L/min", x = "time",title="Partition D")+ ylim(0, 175) + 
  scale_color_gradientn(colours = c("firebrick", "orange", "navyblue"),
                        values = c(0, 0.5, 1),limits = c(50, 120), oob = scales::squish)
#######

df_longdf %>% 
  tabyl(kmlclusters2, time) -> kml2tab

df_longdf %>% 
  tabyl(kmlclusters4, time) -> kml4tab

aggregate(kml2tab, kml4tab)

df_longdf %>%  # concordance
  tabyl(kmlclusters2, kmlclusters4)

df_longdf %>%
  ggplot(aes(x = time, y = v_PA, group = time)) +
  geom_boxplot()



#EDA with v_PA
summary(df_longdf$v_PA)
ggplot(df) +
  geom_histogram(alpha=0.6, binwidth = 2, aes(x = v_PA, colour = v_PA)) +
  geom_density(aes(x = v_PA), alpha=.2, fill="#FF6666") +
  theme(legend.position = "none") +
  labs(x = "MAP mmHg")
  scale_colour_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  geom_vline(aes(xintercept=mean(df$v_PA)),
             color="pink", linetype="dashed", size=1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme_gray() +
  theme(legend.position = "",
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))

# v_GFR
summary(df_longdf$v_GFR0)
ggplot(df_longdf) +
  geom_histogram(alpha=0.6, binwidth = 2, aes(x = v_GFR0.y)) +
  geom_density(aes(x = v_GFR0.y), alpha=.2, fill="#FF6666") +
  theme(legend.position = "none") +
  scale_colour_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  geom_vline(aes(xintercept= 90),
             color="pink", linetype="dashed", size=1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme_gray() +
  labs(x = "GFR L/min") +
  theme(legend.position = "",
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))

ggplot(df_longdf) +
  geom_histogram(aes(x = v_GFR0.y), binwidth = 5)

#+
  geom_density(aes(x = v_GFR0.x), alpha=.2, fill="#FF6666") +
  theme(legend.position = "none") +
  scale_colour_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  geom_vline(aes(xintercept=mean(df$v_GFR0)),
             color="pink", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=90),
             colour = "lightblue", linetype = "dot", size = 1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme_gray()
ggplot(df) +
  geom_density(aes(x = v_GFR0)) +
  theme(legend.position = "none") 

ggplot(df) +
  geom_density(aes(x = v_PA)) +
  theme(legend.position = "none") 

length(df$v_GFR0[df$v_GFR0 < 90])

class(df$p_CV)
class(df$time)
names(df)






