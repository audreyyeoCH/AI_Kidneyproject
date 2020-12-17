
load("doi_10.5061_dryad.h3s0r__v1/virtppl.1h.RData")
load("doi_10.5061_dryad.h3s0r__v1/virtppl.1d.RData")
load("doi_10.5061_dryad.h3s0r__v1/virtppl.1m.RData")
load("doi_10.5061_dryad.h3s0r__v1/virtppl.1w.RData")
load("doi_10.5061_dryad.h3s0r__v1/virtppl.ee.RData")

View(virtppl.1d[1:6,])
View(virtppl.1h[1:6,])


set.seed(1964) #Kamala's birth year
n = 1000
virtppl.1h0 = virtppl.1h[sample(1:nrow(virtppl.1h), n, replace = FALSE),]
virtppl.1d0 = sample(1:nrow(virtppl.1d), n, replace = FALSE)
virtppl.1w0 = sample(1:nrow(virtppl.1w), n, replace = FALSE)
virtppl.1m0 = sample(1:nrow(virtppl.1m), n, replace = FALSE)

virtppl.1h0$time = data.frame(rep("1", 1000))
virtppl.1d0$time = data.frame(rep("2", 1000))
virtppl.1w0$time = data.frame(rep("3", 1000))
virtppl.1m0$time = data.frame(rep("4", 1000))

# data frame for 1 hour


datafame = data.frame(rbind(virtppl.1h0, virtppl.1d0, virtppl.1w0, virtppl.1m0))
out = NA
for (i in 1:4)) { datafame[i]$id = data.frame(c(1:nrow(virtppl.1d))) }
  #datafame = c(datafame$id, datafame)

