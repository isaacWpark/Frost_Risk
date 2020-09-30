library("optimx")
library("numDeriv")
library("RCurl") 
library("lme4")
library("data.table")

# Set working directoy
setwd("YOUR DIRECTORY")

# Load specimen-level data
dat_master_norm <- fread("./Data/SpecimenData.csv")


# Load species-level data
dat_species <- fread("./Data/Supplementary Table 1.csv")

# Rename column with species names
colnames(dat_species)[2] <- "Accepted_name_species"



#
# ::::::::::::::::::::::::::::: Caluclate temporal trends in dates of last frost ::::::::::::::::::::::::::
#




# Fit mixed effect model for dates of last frost vs. year (Equation 2 of main text)

mod <-
  
  lmer(data = dat_master_norm, REML = T, control=lmerControl(optimizer="bobyqa"),
       
       
       
       bFFP_AnnDev ~  
         
         scale(Year)+
         scale(bFFP_Normal)+
         (1+scale(Year)|Accepted_name_species)+
         (1|Accepted_name_genus)+
         (1|Accepted_name_family))

# Note: parameters are centered and scaled to aid convergence of the model



# Assess convergence
relgrad <- with(mod@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))


# Dataframe with coefficients from model
mod.coef <- coef(mod)


#
# Merge coefficients into species-level dataset for storage
#

# Change variable name in model output data.frame
colnames(mod.coef$Accepted_name_species)[2] <- c("year_bFFP_AnnDev_glmm")
# Make variable with species names
mod.coef$Accepted_name_species$Accepted_name_species <- rownames(mod.coef$Accepted_name_species)

# Merge coefficients for year from GLMM (temporal trends) into species-level dataset by species name
dat_species <- merge(dat_species, mod.coef$Accepted_name_species[,c("Accepted_name_species","year_bFFP_AnnDev_glmm")], by="Accepted_name_species")


# Recover scale of data dividing by SD of year
dat_species$year_bFFP_AnnDev_glmm <- dat_species$year_bFFP_AnnDev_glmm/sd(dat_master_norm[, Year])

















# Fit mixed effect model for dates of last frost vs. year (Equation 2 of main text)


mod <-
  
  lmer(data = dat_master_norm, REML = T, control=lmerControl(optimizer="bobyqa"),
       
       
       
       DOY ~  
         
         scale(Year)+
         scale(bFFP_Normal)+
         (1+scale(Year)|Accepted_name_species)+
         (1|Accepted_name_genus)+
         (1|Accepted_name_family))




# Assess convergence
relgrad <- with(mod@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))


# Dataframe with coefficients from model
mod.coef <- coef(mod)



#
# Merge coefficients into species-level dataset for storage
#


# Change variable name in model output data.frame
colnames(mod.coef$Accepted_name_species)[2] <- c("year_doy_glmm")
# Make variable with species names
mod.coef$Accepted_name_species$Accepted_name_species <- rownames(mod.coef$Accepted_name_species)

# Merge coefficients for year from GLMM (temporal trends) into species-level dataset by species name
dat_species <- merge(dat_species, mod.coef$Accepted_name_species[,c("Accepted_name_species","year_doy_glmm")], by="Accepted_name_species")


# Recover scale of data dividing by SD of year
dat_species$year_doy_glmm <- dat_species$year_doy_glmm/sd(dat_master_norm[, Year])





