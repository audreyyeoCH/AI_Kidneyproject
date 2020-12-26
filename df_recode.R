# load libraries
library(tidyverse)
library(viridis)
library(ggmosaic)
library(janitor)
library(kml)

# load virt pop data
# 384'000 rows x 412 variables, wide form, each row is an individual 
load("doi_10.5061_dryad.h3s0r__v1/virtppl.1h.RData")
load("doi_10.5061_dryad.h3s0r__v1/virtppl.1d.RData")
load("doi_10.5061_dryad.h3s0r__v1/virtppl.1w.RData")
load("doi_10.5061_dryad.h3s0r__v1/virtppl.1m.RData")
#load("doi_10.5061_dryad.h3s0r__v1/virtppl.ee.RData") # elementary effects

# all stars of "folklore" by taylor swift
betty = length(virtppl.1h$v_GFR[virtppl.1h$v_GFR <= 90/1000]) # 2834
# in the 384'000 observations, there are 2'834 observations with GFR below 90mL/min
ines = length(virtppl.1d$v_GFR[virtppl.1d$v_GFR <= 90/1000]) # 2771
james = length(virtppl.1w$v_GFR[virtppl.1w$v_GFR <= 90/1000]) # 2765
august = length(virtppl.1m$v_GFR[virtppl.1m$v_GFR <= 90/1000]) #2781

# getting the verge of kidney failures
# h9 = virtppl.1h[virtppl.1h$v_GFR <= 90/1000,]
# d9 = virtppl.1d[virtppl.1d$v_GFR <= 90/1000,]
# w9 = virtppl.1w[virtppl.1w$v_GFR <= 90/1000,]
# m9 = virtppl.1m[virtppl.1m$v_GFR <= 90/1000,]
# 
# h10 =virtppl.1h[virtppl.1h$v_GFR > 90/1000,]
# d10 = virtppl.1d[virtppl.1d$v_GFR > 90/1000,]
# w10 = virtppl.1w[virtppl.1w$v_GFR > 90/1000,]
# m10 =virtppl.1m[virtppl.1m$v_GFR > 90/1000,]

# preview
# View(virtppl.1d[1:6,])
# View(virtppl.1h[1:6,])


# assign ID
virtppl.1h$id = c(seq(1,nrow(virtppl.1h),1))
virtppl.1d$id = c(seq(1,nrow(virtppl.1h),1))
virtppl.1w$id = c(seq(1,nrow(virtppl.1h),1))
virtppl.1m$id = c(seq(1,nrow(virtppl.1h),1))
#check
summary(virtppl.1d$id)

# sampling n out of data frame
set.seed(1964) # Kamala Harris' Birthyear
n = 2500
sampleit = as.numeric(c(sample(1:nrow(virtppl.1h), n, replace = FALSE)))
# randomly sample from virt population dataset, slow
virtppl.1h0 = virtppl.1h[virtppl.1h[virtppl.1h$v_GFR>90,]$id == sampleit | virtppl.1h$v_GFR <= 90/1000,]
virtppl.1d0 = virtppl.1d[virtppl.1d$id == sampleit | virtppl.1d$v_GFR <= 90/1000,]
virtppl.1w0 = virtppl.1w[virtppl.1w$id == sampleit | virtppl.1w$v_GFR <= 90/1000,]
virtppl.1m0 = virtppl.1m[virtppl.1m$id == sampleit | virtppl.1m$v_GFR <= 90/1000,]

# ########

#assign GRP group
virtppl.1h$group = ifelse(virtppl.1h$v_GFR <= 90/1000 , "low GFR", "norm GFR")
virtppl.1d$group = ifelse(virtppl.1d$v_GFR <= 90/1000 , "low GFR", "norm GFR")
virtppl.1w$group = ifelse(virtppl.1w$v_GFR <= 90/1000 , "low GFR", "norm GFR")
virtppl.1m$group = ifelse(virtppl.1m$v_GFR <= 90/1000 , "low GFR", "norm GFR")

# assign Time var
virtppl.1h$time = data.frame(rep(1, nrow(virtppl.1h)))
virtppl.1d$time = data.frame(rep(2, nrow(virtppl.1d)))
virtppl.1w$time = data.frame(rep(3, nrow(virtppl.1w)))
virtppl.1m$time = data.frame(rep(4, nrow(virtppl.1m)))

#check
summary(virtppl.1d$id)

# ckd and non ckd population
low_1h = virtppl.1h[virtppl.1h$group == "low GFR",]
low_1d = virtppl.1h[virtppl.1d$group == "low GFR",]
low_1w = virtppl.1h[virtppl.1w$group == "low GFR",]
low_1m =virtppl.1h[virtppl.1m$group == "low GFR",]

high_1h0 = virtppl.1h[virtppl.1h$group == "norm GFR",]
high_1d0 = virtppl.1h[virtppl.1d$group == "norm GFR",]
high_1w0 = virtppl.1h[virtppl.1w$group == "norm GFR",]
high_1m0 = virtppl.1h[virtppl.1m$group == "norm GFR",]

# sampling n out of data frame
set.seed(1964) # Kamala Harris' Birthyear
n = mean(betty, ines, james, august)
sampleit = as.numeric(c(sample(1:nrow(virtppl.1h), n, replace = FALSE)))

# subset the sampleit data
high_1h = high_1h0[high_1h0$id %in% sampleit,]
high_1d = high_1d0[high_1d0$id %in% sampleit,]
high_1w = high_1w0[high_1w0$id %in% sampleit,]
high_1m = high_1m0[high_1m0$id %in% sampleit,]

# saving
save(low_1h, file = "doi_10.5061_dryad.h3s0r__v1/low_1h.RData")
save(low_1d, file = "doi_10.5061_dryad.h3s0r__v1/low_1d.RData")
save(low_1w, file = "doi_10.5061_dryad.h3s0r__v1/low_1w.RData")
save(low_1m, file = "doi_10.5061_dryad.h3s0r__v1/low_1m.RData")

save(high_1h, file = "doi_10.5061_dryad.h3s0r__v1/high_1h.RData")
save(high_1d, file = "doi_10.5061_dryad.h3s0r__v1/high_1d.RData")
save(high_1w, file = "doi_10.5061_dryad.h3s0r__v1/high_1w.RData")
save(high_1m, file = "doi_10.5061_dryad.h3s0r__v1/high_1m.RData")

# save
save(high_1h0, file = "doi_10.5061_dryad.h3s0r__v1/high_1h0.RData")
save(high_1d0,file = "doi_10.5061_dryad.h3s0r__v1/high_1d0.RData")
save(high_1w0, file = "doi_10.5061_dryad.h3s0r__v1/high_1w0.RData")
save(high_1m0, file = "doi_10.5061_dryad.h3s0r__v1/high_1m0.RData")

#load
load(file = "doi_10.5061_dryad.h3s0r__v1/high_1h0.RData")
load(file = "doi_10.5061_dryad.h3s0r__v1/high_1d0.RData")
load(file = "doi_10.5061_dryad.h3s0r__v1/high_1w0.RData")
load(file = "doi_10.5061_dryad.h3s0r__v1/high_1m0.RData")

# Load
load(file = "doi_10.5061_dryad.h3s0r__v1/low_1h.RData")
load(file = "doi_10.5061_dryad.h3s0r__v1/low_1d.RData")
load(file = "doi_10.5061_dryad.h3s0r__v1/low_1w.RData")
load(file = "doi_10.5061_dryad.h3s0r__v1/low_1m.RData")

load(file = "doi_10.5061_dryad.h3s0r__v1/high_1h.RData")
load(file = "doi_10.5061_dryad.h3s0r__v1/high_1d.RData")
load(file = "doi_10.5061_dryad.h3s0r__v1/high_1w.RData")
load(file = "doi_10.5061_dryad.h3s0r__v1/high_1m.RData")

hey = rbind(low_1h, high_1h)
hey2 = rbind(low_1d, high_1d)
hey3 = rbind(low_1w, high_1w)
hey4 = rbind(low_1m, high_1m)

#hey5 = data.frame(as.matrix(hey), as.matrix(hey2))

huh = data.frame(rbind(as.matrix(hey), as.matrix(hey2)))
huh2 = data.frame(rbind(as.matrix(huh), as.matrix(hey3)))
huh3 = data.frame(rbind(as.matrix(huh2), as.matrix(hey4)))

 #df = data.frame(as.matrix(hey), as.matrix(hey2), 
                        # as.matrix(hey3), as.matrix(hey4))

# aggregate to df
#df = data.frame(rbind(as.matrix(high_1d0), as.matrix(high_1h0), as.matrix(high_1w0), as.matrix(high_1m0), as.matrix(low_1h),as.matrix(low_1d),as.matrix(low_1w),as.matrix(low_1m)), .id = NULL)
# recode_GFR, conversion from L/min to mL/min
huh3$v_PA = as.numeric(as.character(huh3$v_PA))
huh3$v_GFR = as.numeric(as.character(huh3$v_GFR))
huh3$v_GFR0 <- huh3$v_GFR*1000
#df$v_PA <- as.numeric(df$v_PA)
#df$time <- as.factor(df$time)

# save
save(huh3, file = "doi_10.5061_dryad.h3s0r__v1/df.RData")
# load
load("doi_10.5061_dryad.h3s0r__v1/df.RData")

# sampleit0 = as.numeric(c(sample(1:22386, 2000, replace = FALSE)))
# df$id = as.numeric(df$id)
# df0 = df[ df$id %in% sampleit0,]
# class(sampleit0)
# class(df$id)
# save(df0, file = "doi_10.5061_dryad.h3s0r__v1/df0.RData")
#summary tables
# tab.n = array(NA, dim=c(4,3))
# tab.n[,1] = levels(df$time)
# tab.n[,2] = summary(df$time)
# tab.n[,3] = tapply(df$time, df$group,
#                     function (x) sprintf("%0.2f (%0.2f)", mean(x), sd(x)))
# colnames(tab.n) = c("time point", "low GFR", "high GFR")
# print(tab.dv)


#EDA
summary(df$v_PA)

ggplot(df) +
  geom_histogram(alpha = 0.6, binwidth= 2, aes(x = v_PA)) +
  geom_vline(aes(xintercept= 106.6), color="pink", 
             linetype="dashed", size=1)

ggplot(df) +
  geom_histogram(alpha=0.6, binwidth = 20, 
                 aes(x = v_GFR0, colour = v_GFR0)) +
  geom_vline(aes(xintercept= 90), color="pink", 
             linetype="dashed", size=1)

# first longitudinal plot
ggplot(df) +
  geom_path(aes(y = v_PA, x = time, group = , colour = as.factor(id))) +
  theme(legend.position = "none") + labs(y = "log MAP mmHG") 

ggplot(df) + # shows missing values
  geom_path(aes(y = log(v_GFR0), x = time, group = , colour = as.factor(id))) +
  theme(legend.position = "none") + labs(y = "log GFR L/min") 

ggplot(df) +
  geom_path(aes(y = v_GFR0, x = time, group = , colour = as.factor(id))) +
  theme(legend.position = "none")

ggplot(df) +
  geom_path(aes(y = v_AU1, x = time, group = , colour = as.factor(id))) +
  theme(legend.position = "none")

ggplot(df) +
  geom_path(aes(y = log(df$p_AARK), x = time, group = , colour = as.factor(id))) +
  theme(legend.position = "none") 

ggplot(df) +
  geom_path(aes(y = log(df$v_GFR), x = time, group = , colour = as.factor(id))) +
  theme(legend.position = "none") 
  
# kml clustering



# datafame = data.frame(rbind(virtppl.1h0, virtppl.1d0, virtppl.1w0, virtppl.1m0))
# out = NA
# for (i in 1:4) { datafame[i]$id = data.frame(c(1:nrow(virtppl.1d))) }
#   #datafame = c(datafame$id, datafame)
# for (i in 1:4) {
#   assign(paste0("df", sep = "", i), 
#          as.data.frame(datafame[i]))
# }



######## Cld object needed for kml #####
shortdf <- df[, c(413, 415, 416)]
str(shortdf)
spread(shortdf, id, time, v_GFR0) -> widedf0
head(widedf0)
sum(is.na(widedf0)) # 6461 missing values of (11159 x 415)
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

View(df_longdf[1:10,415:416])
#colnames(df_longdf)[415] -> "v_GRF0"
save(df_longdf, file = "df_longdf.Rda")

# EDA, important variables for output GFR are:
#df$p_AARK
#df$p_EARK
#df$p_GFLC
#df$p_NID



df_longA = df_longdf[kmlclusters == "A"]

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

ggplot(df_longdf, aes(x = time, y = v_GFR0.y, colour = kmlclusters4, group = kmlclusters4)) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_gray()+
  labs(y = "GFR mL/min", x = "time") +
  #geom_vline(xintercept = c(6, ), colour = "red", linetype = "dotted") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.text.x.bottom = element_text(size = 8)) 

# v_PA
ggplot(df_longdf, aes(x = time, y = v_PA, colour = kmlclusters2, group = kmlclusters2)) +
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
  scale_colour_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  geom_vline(aes(xintercept=mean(df$v_PA)),
             color="pink", linetype="dashed", size=1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme_gray()

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
  theme_gray()

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










