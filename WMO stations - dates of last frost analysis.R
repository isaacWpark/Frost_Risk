library(data.table)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(Cairo)

# Set working directory
setwd("YOUR WD")

# Load bFFP annual data series for different temperature thresholds
bFFP0 <- read.csv("./Data/bFFP below 0.csv")
bFFP1 <- read.csv("./Data/bFFP below 1.csv")
bFFP2 <- read.csv("./Data/bFFP below 2.csv")
bFFP3 <- read.csv("./Data/bFFP below 3.csv")
bFFP4 <- read.csv("./Data/bFFP below 4.csv")

# set variable name for column with years in all datasets
colnames(bFFP0)[802] = colnames(bFFP1)[802] = colnames(bFFP2)[802] =colnames(bFFP3)[802] =colnames(bFFP4)[802] <- "year"


#
# Quality control
#


# Mark all cells that have values (non-NAs) for both 0 C AND each of the other thresholds 
bFFP01_na <-as.data.frame( is.na(bFFP0[,1:801]) == F & is.na(bFFP1[,1:801]) == F )
bFFP02_na <- as.data.frame( is.na(bFFP0[,1:801]) == F & is.na(bFFP2[,1:801]) == F )
bFFP03_na <- as.data.frame( is.na(bFFP0[,1:801]) == F & is.na(bFFP3[,1:801]) == F )
bFFP04_na <- as.data.frame( is.na(bFFP0[,1:801]) == F & is.na(bFFP4[,1:801]) == F )

# Mark all cells that have values (non-NAs) for each of the other thresholds 
bFFP0_na <-as.data.frame( is.na(bFFP0[,1:801]) == F)
bFFP1_na <-as.data.frame( is.na(bFFP1[,1:801]) == F)
bFFP2_na <- as.data.frame( is.na(bFFP2[,1:801]) == F)
bFFP3_na <- as.data.frame( is.na(bFFP3[,1:801]) == F)
bFFP4_na <- as.data.frame( is.na(bFFP4[,1:801]) == F)


# Compute number of years with data for both 0 deg C and each other treshold
vobs_01 <- unlist(lapply(bFFP01_na, sum))
vobs_02 <- unlist(lapply(bFFP02_na, sum))
vobs_03 <- unlist(lapply(bFFP03_na, sum))
vobs_04 <- unlist(lapply(bFFP04_na, sum))

# Extrac column indexes for stations with 30+ years of obs for each treshold
vobs_0 <- unlist(lapply(bFFP0_na, sum))
vobs_1 <- unlist(lapply(bFFP1_na, sum))
vobs_2 <- unlist(lapply(bFFP2_na, sum))
vobs_3 <- unlist(lapply(bFFP3_na, sum))
vobs_4 <- unlist(lapply(bFFP4_na, sum))


# Extract column indexes for stations that have 30+ years of observations for B0TH 0 deg C and each other treshold
vcols_01 <- which(vobs_01>29)
vcols_02 <- which(vobs_02>29)
vcols_03 <- which(vobs_03>29)
vcols_04 <- which(vobs_04>29)

# Extract column indexes for stations that have 30+ years of observations for B0TH 0 deg C and each other treshold
vcols_0 <- which(vobs_0>29)
vcols_1 <- which(vobs_1>29)
vcols_2 <- which(vobs_2>29)
vcols_3 <- which(vobs_3>29)
vcols_4 <- which(vobs_4>29)












# Create functions to estimate bFFP vs year trends for all thresholds

# 0 deg
lm.func_0 <-  function(x){sum_i <- summary(
  
  lm(bFFP0[,x] ~  bFFP0$year))

coefs <- sum_i$coefficient[2,1]
p_vals <- sum_i$coefficient[2,4]
rsq <- sum_i$r.squared

list(coefs[1],
     p_vals[1],rsq)

}

# -1 deg C
lm.func_1 <-  function(x){sum_i <- summary(
  
  lm(bFFP1[,x] ~  bFFP1$year))

coefs <- sum_i$coefficient[2,1]
p_vals <- sum_i$coefficient[2,4]
rsq <- sum_i$r.squared

list(coefs[1],
     p_vals[1],rsq)

}

# -2 deg C
lm.func_2 <-  function(x){sum_i <- summary(
  
  lm(bFFP2[,x] ~  bFFP2$year))

coefs <- sum_i$coefficient[2,1]
p_vals <- sum_i$coefficient[2,4]
rsq <- sum_i$r.squared

list(coefs[1],
     p_vals[1],rsq)

}

# -3 deg C
lm.func_3 <-  function(x){sum_i <- summary(
  
  lm(bFFP3[,x] ~  bFFP3$year))

coefs <- sum_i$coefficient[2,1]
p_vals <- sum_i$coefficient[2,4]
rsq <- sum_i$r.squared

list(coefs[1],
     p_vals[1],rsq)

}

#-4 deg C
lm.func_4 <-  function(x){sum_i <- summary(
  
  lm(bFFP4[,x] ~  bFFP4$year))

coefs <- sum_i$coefficient[2,1]
p_vals <- sum_i$coefficient[2,4]
rsq <- sum_i$r.squared

list(coefs[1],
     p_vals[1],rsq)

}

# For each T threshold, apply models to all stations with 30+ years of available data 
bFFP0_yr <- lapply(vcols_0, lm.func_0)
bFFP1_yr <- lapply(vcols_1, lm.func_1)
bFFP2_yr <- lapply(vcols_2, lm.func_2)
bFFP3_yr <- lapply(vcols_3, lm.func_3)
bFFP4_yr <- lapply(vcols_4, lm.func_4)


# Extract trends (every third argument) from list output. EV
bFFP0_yr <- as.data.frame ( unlist(bFFP0_yr)[seq(1,3*length(bFFP0_yr)-2, by=3)] )
colnames(bFFP0_yr) <- "bFFP0_yr"
bFFP1_yr <- as.data.frame ( unlist(bFFP1_yr)[seq(1,3*length(bFFP1_yr)-2, by=3)] )
colnames(bFFP1_yr) <- "bFFP1_yr"
bFFP2_yr <- as.data.frame (  unlist(bFFP2_yr)[seq(1,3*length(bFFP2_yr)-2, by=3)] )
colnames(bFFP2_yr) <- "bFFP2_yr"
bFFP3_yr <- as.data.frame (  unlist(bFFP3_yr)[seq(1,3*length(bFFP3_yr)-2, by=3)] )
colnames(bFFP3_yr) <- "bFFP3_yr"
bFFP4_yr <- as.data.frame (  unlist(bFFP4_yr)[seq(1,3*length(bFFP4_yr)-2, by=3)] )
colnames(bFFP4_yr) <- "bFFP4_yr"




#
# Create a synthetic dataset
#

# Import station info
dat_wmo <- fread("./Data/WMO station data - 1920 to 2016 - INFO.csv")

# Create dataset
dat_bFFP <- data.table(matrix(nrow=801))

# Station names for:

# Synthetic dataset
dat_bFFP$stations <- unlist(strsplit(colnames(bFFP0), split= "_"))[seq(1,801*2 -1, by=2)]

# Correlation datasets
cor01$stations <- unlist(strsplit(rownames(cor01), split= "_"))[seq(1,nrow(cor01)*2 -1, by=2)]
cor02$stations <- unlist(strsplit(rownames(cor02), split= "_"))[seq(1,nrow(cor02)*2 -1, by=2)]
cor03$stations <- unlist(strsplit(rownames(cor03), split= "_"))[seq(1,nrow(cor03)*2 -1, by=2)]
cor04$stations <- unlist(strsplit(rownames(cor04), split= "_"))[seq(1,nrow(cor04)*2 -1, by=2)]

# Temporal trends dataset
bFFP0_yr$stations <- unlist(strsplit(rownames(bFFP0_yr), split= "_"))[seq(1,nrow(bFFP0_yr)*2 -1, by=2)]
bFFP1_yr$stations <- unlist(strsplit(rownames(bFFP1_yr), split= "_"))[seq(1,nrow(bFFP1_yr)*2 -1, by=2)]
bFFP2_yr$stations <- unlist(strsplit(rownames(bFFP2_yr), split= "_"))[seq(1,nrow(bFFP2_yr)*2 -1, by=2)]
bFFP3_yr$stations <- unlist(strsplit(rownames(bFFP3_yr), split= "_"))[seq(1,nrow(bFFP3_yr)*2 -1, by=2)]
bFFP4_yr$stations <- unlist(strsplit(rownames(bFFP4_yr), split= "_"))[seq(1,nrow(bFFP4_yr)*2 -1, by=2)]



# Merge cor datasets
dat_bFFP <- merge(dat_bFFP, cor01, by='stations', all.x=T)
dat_bFFP <- merge(dat_bFFP, cor02, by='stations', all.x=T)
dat_bFFP <- merge(dat_bFFP, cor03, by='stations', all.x=T)
dat_bFFP <- merge(dat_bFFP, cor04, by='stations', all.x=T)

# Merge temporal trends dataset
dat_bFFP <- merge(dat_bFFP, bFFP0_yr, by='stations', all.x = T)
dat_bFFP <- merge(dat_bFFP, bFFP1_yr, by='stations', all.x = T)
dat_bFFP <- merge(dat_bFFP, bFFP2_yr, by='stations', all.x = T)
dat_bFFP <- merge(dat_bFFP, bFFP3_yr, by='stations', all.x = T)
dat_bFFP <- merge(dat_bFFP, bFFP4_yr, by='stations', all.x = T)

# Merge dat_wmo info
colnames(dat_wmo)[1] <- "stations"
dat_bFFP <- merge(dat_bFFP, dat_wmo[,1:6], by='stations', all.x = T)







# ::::::::::::::::::::::::::::::: ANALYSIS AND VISUALIZATION


#
# Bivariate plots of temporal trends
#

# Change in 0 C vs -1 C

plot_yr_01 <-

ggplot(data = dat_bFFP, aes(x=10*bFFP0_yr, y=10*bFFP1_yr)) + theme_bw(base_size = 16)+
  

  scale_x_continuous(name = "0°C bFFP change (Days/Decade)", limits = c(-6.3, 5), breaks = c(-6,-3,0,3), labels = c(-6,-3,0,3))+
  scale_y_continuous(name = "-1°C bFFP change (Days/Decade)", breaks = c(-6,-3,0,3), labels = c(-6,-3,0,3))+
  
  geom_abline(slope=1)+
  geom_point()+
  geom_smooth(method='lm', color='red', fill='tomato1' )+
  
  theme(panel.grid = element_blank())

plot_yr_01


# Change in 0 C vs -2 C


plot_yr_02 <-

ggplot(data = dat_bFFP, aes(x=10*bFFP0_yr, y=10*bFFP2_yr)) + theme_bw(base_size = 16)+
  

  scale_x_continuous(name = "0°C bFFP change (Days/Decade)", limits = c(-6.3, 5), breaks = c(-6,-3,0,3), labels = c(-6,-3,0,3))+
  scale_y_continuous(name = "-2°C bFFP change (Days/Decade)", limits = c(-6.3, 5), breaks = c(-6,-3,0,3), labels = c(-6,-3,0,3))+
  
  geom_abline(slope=1)+
  geom_point()+
  geom_smooth(method='lm', color='red', fill='tomato1')+
  
  
  theme(panel.grid = element_blank())

plot_yr_02


# Change in 0 C vs -3 C

plot_yr_03 <-
  
  ggplot(data = dat_bFFP, aes(x=10*bFFP0_yr, y=10*bFFP3_yr)) + theme_bw(base_size = 16)+
  

  scale_x_continuous(name = "0°C bFFP change (Days/Decade)", limits = c(-6.3, 5), breaks = c(-6,-3,0,3), labels = c(-6,-3,0,3))+
  scale_y_continuous(name = "-3°C bFFP change (Days/Decade)", limits = c(-6.3, 5), breaks = c(-6,-3,0,3), labels = c(-6,-3,0,3))+
  
  geom_abline(slope=1)+
  geom_point()+
  geom_smooth(method='lm', color='red', fill='tomato1')+
  
  theme(panel.grid = element_blank())

plot_yr_03


# Change in 0 C vs -4 C


plot_yr_04 <-
  
  ggplot(data = dat_bFFP, aes(x=10*bFFP0_yr, y=10*bFFP4_yr)) + theme_bw(base_size = 16)+
  

  scale_x_continuous(name = "0°C bFFP change (Days/Decade)", limits = c(-6.3, 5), breaks = c(-6,-3,0,3), labels = c(-6,-3,0,3))+
  scale_y_continuous(name = "-4°C bFFP change (Days/Decade)", limits = c(-6.3, 5), breaks = c(-6,-3,0,3), labels = c(-6,-3,0,3))+
  
  geom_abline(slope=1)+
  geom_point()+
  geom_smooth(method='lm', color='red', fill='tomato1')+
  
  
  theme(panel.grid = element_blank())

plot_yr_04


#
# Export composite graph
#

Cairo(file= "bFFP - trends 2.png",             # ENTER NAME OF PLOT
      type="png",
      units="cm", 
      width=22,
      height=22, 
      pointsize=12, 
      dpi=600)

ggarrange(plot_yr_01, plot_yr_02, plot_yr_03, plot_yr_04, nrow = 2, ncol = 2)


dev.off()




