################################################################################
## DATA SIMULATION FOR DEMO CFA ANALYSIS                                      ##
##                                                                            ##
## Based on method by Rocabado et al. (2020) in                               ##
## https://doi.org/10.1039/D0RP00025F                                         ##
################################################################################

## Author: Jeffrey R. Raker, Ph.D.
## Released: 2022
## Last Updated: 2022-10-17

################################################################################

### THIS SECTION IS FOR LOADING NECESSARY PACKAGES ###

## Install packages (and dependencies) if necessary
# install.packages("simstandard", dependencies = TRUE)
# install.packages("xlsx", dependencies = TRUE)

## The "simstandard" package is used to simulate data.
library(simstandard)

## The "xlsx" package is used to export simulated data to an Excel file type.
library (xlsx)

################################################################################

### THIS SECTION CREATES PROTOTYICAL MODELS FOR DATA SIMULATION ###

## Each model has four factors with four items per factor.

## Model 1: All items have the same factor loadings with varied
##          correlations between factors.
SIMMOD1 <-'
        F1 =~ 0.8*Q1 + 0.8*Q2 + 0.8*Q3 + 0.8*Q4
        F2 =~ 0.8*Q5 + 0.8*Q6 + 0.8*Q7 + 0.8*Q8
        F3 =~ 0.8*Q9 + 0.8*Q10 + 0.8*Q11 + 0.8*Q12
        F4 =~ 0.8*Q13 + 0.8*Q14 + 0.8*Q15 + 0.8*Q16
        
        F1 =~ 0.4*F2
        F1 =~ 0.4*F3
        F1 =~ 0.4*F4
        F2 =~ 0.3*F3
        F2 =~ 0.3*F4
        F3 =~ 0.2*F4
       '

## Model 2: Factor loadings are varied compared to Model 1 with the same
##          correlations between factors as Model 1.
SIMMOD2 <-'
        F1 =~ 0.8*Q1 + 0.75*Q2 + 0.8*Q3 + 0.8*Q4
        F2 =~ 0.8*Q5 + 0.8*Q6 + 0.75*Q7 + 0.8*Q8
        F3 =~ 0.8*Q9 + 0.8*Q10 + 0.8*Q11 + 0.9*Q12
        F4 =~ 0.8*Q13 + 0.9*Q14 + 0.8*Q15 + 0.8*Q16
        
        F1 =~ 0.4*F2
        F1 =~ 0.4*F3
        F1 =~ 0.4*F4
        F2 =~ 0.3*F3
        F2 =~ 0.3*F4
        F3 =~ 0.2*F4
       '

## Model 3: Factor loadings are further varied compared to Model 1 with
##          additionally varied correlations between factors compared 
##          to Model 1.
SIMMOD3 <-'
        F1 =~ 0.5*Q1 + 0.8*Q2 + 0.8*Q3 + 0.8*Q4
        F2 =~ 0.5*Q5 + 0.8*Q6 + 0.8*Q7 + 0.8*Q8
        F3 =~ 0.8*Q9 + 0.8*Q10 + 0.8*Q11 + 0.9*Q12
        F4 =~ 0.8*Q13 + 0.9*Q14 + 0.8*Q15 + 0.8*Q16
        
        F1 =~ 0.5*F2
        F1 =~ 0.5*F3
        F1 =~ 0.5*F4
        F2 =~ 0.2*F3
        F2 =~ 0.2*F4
        F3 =~ 0.1*F4
       '

################################################################################

# Simulate the data based on the entered model.

# Set seed to ensure reproducibility
set.seed(119978)

# Create data for three groups
my.data.grp1 <- sim_standardized(SIMMOD1, n = 1000, observed = T, latent = F, errors = F)
my.data.grp2 <- sim_standardized(SIMMOD2, n = 1000, observed = T, latent = F, errors = F)
my.data.grp3 <- sim_standardized(SIMMOD3, n = 1000, observed = T, latent = F, errors = F)

# Adjust means for several items
my.data.grp1$Q4 <- my.data.grp1$Q4 - 1
my.data.grp1$Q12 <- my.data.grp1$Q12 + 1
my.data.grp1$Q9 <- my.data.grp1$Q9 + 2
my.data.grp2$Q15 <- my.data.grp2$Q15 + 1
my.data.grp2$Q5 <- my.data.grp2$Q5 - 2

# Transform data from continuous to ordinal
grp1.ord <- my.data.grp1
grp1.ord$Q1 <- as.numeric(cut(my.data.grp1$Q1, breaks = 5))
grp1.ord$Q2 <- as.numeric(cut(my.data.grp1$Q2, breaks = 5))
grp1.ord$Q3 <- as.numeric(cut(my.data.grp1$Q3, breaks = 5))
grp1.ord$Q4 <- as.numeric(cut(my.data.grp1$Q4, breaks = 5))
grp1.ord$Q5 <- as.numeric(cut(my.data.grp1$Q5, breaks = 5))
grp1.ord$Q6 <- as.numeric(cut(my.data.grp1$Q6, breaks = 5))
grp1.ord$Q7 <- as.numeric(cut(my.data.grp1$Q7, breaks = 5))
grp1.ord$Q8 <- as.numeric(cut(my.data.grp1$Q8, breaks = 5))
grp1.ord$Q9 <- as.numeric(cut(my.data.grp1$Q9, breaks = 5))
grp1.ord$Q10 <- as.numeric(cut(my.data.grp1$Q10, breaks = 5))
grp1.ord$Q11 <- as.numeric(cut(my.data.grp1$Q11, breaks = 5))
grp1.ord$Q12 <- as.numeric(cut(my.data.grp1$Q12, breaks = 5))
grp1.ord$Q13 <- as.numeric(cut(my.data.grp1$Q13, breaks = 5))
grp1.ord$Q14 <- as.numeric(cut(my.data.grp1$Q14, breaks = 5))
grp1.ord$Q15 <- as.numeric(cut(my.data.grp1$Q15, breaks = 5))
grp1.ord$Q16 <- as.numeric(cut(my.data.grp1$Q16, breaks = 5))

grp2.ord <- my.data.grp2
grp2.ord$Q1 <- as.numeric(cut(my.data.grp2$Q1, breaks = 5))
grp2.ord$Q2 <- as.numeric(cut(my.data.grp2$Q2, breaks = 5))
grp2.ord$Q3 <- as.numeric(cut(my.data.grp2$Q3, breaks = 5))
grp2.ord$Q4 <- as.numeric(cut(my.data.grp2$Q4, breaks = 5))
grp2.ord$Q5 <- as.numeric(cut(my.data.grp2$Q5, breaks = 5))
grp2.ord$Q6 <- as.numeric(cut(my.data.grp2$Q6, breaks = 5))
grp2.ord$Q7 <- as.numeric(cut(my.data.grp2$Q7, breaks = 5))
grp2.ord$Q8 <- as.numeric(cut(my.data.grp2$Q8, breaks = 5))
grp2.ord$Q9 <- as.numeric(cut(my.data.grp2$Q9, breaks = 5))
grp2.ord$Q10 <- as.numeric(cut(my.data.grp2$Q10, breaks = 5))
grp2.ord$Q11 <- as.numeric(cut(my.data.grp2$Q11, breaks = 5))
grp2.ord$Q12 <- as.numeric(cut(my.data.grp2$Q12, breaks = 5))
grp2.ord$Q13 <- as.numeric(cut(my.data.grp2$Q13, breaks = 5))
grp2.ord$Q14 <- as.numeric(cut(my.data.grp2$Q14, breaks = 5))
grp2.ord$Q15 <- as.numeric(cut(my.data.grp2$Q15, breaks = 5))
grp2.ord$Q16 <- as.numeric(cut(my.data.grp2$Q16, breaks = 5))

grp3.ord <- my.data.grp3
grp3.ord$Q1 <- as.numeric(cut(my.data.grp3$Q1, breaks = 5))
grp3.ord$Q2 <- as.numeric(cut(my.data.grp3$Q2, breaks = 5))
grp3.ord$Q3 <- as.numeric(cut(my.data.grp3$Q3, breaks = 5))
grp3.ord$Q4 <- as.numeric(cut(my.data.grp3$Q4, breaks = 5))
grp3.ord$Q5 <- as.numeric(cut(my.data.grp3$Q5, breaks = 5))
grp3.ord$Q6 <- as.numeric(cut(my.data.grp3$Q6, breaks = 5))
grp3.ord$Q7 <- as.numeric(cut(my.data.grp3$Q7, breaks = 5))
grp3.ord$Q8 <- as.numeric(cut(my.data.grp3$Q8, breaks = 5))
grp3.ord$Q9 <- as.numeric(cut(my.data.grp3$Q9, breaks = 5))
grp3.ord$Q10 <- as.numeric(cut(my.data.grp3$Q10, breaks = 5))
grp3.ord$Q11 <- as.numeric(cut(my.data.grp3$Q11, breaks = 5))
grp3.ord$Q12 <- as.numeric(cut(my.data.grp3$Q12, breaks = 5))
grp3.ord$Q13 <- as.numeric(cut(my.data.grp3$Q13, breaks = 5))
grp3.ord$Q14 <- as.numeric(cut(my.data.grp3$Q14, breaks = 5))
grp3.ord$Q15 <- as.numeric(cut(my.data.grp3$Q15, breaks = 5))
grp3.ord$Q16 <- as.numeric(cut(my.data.grp3$Q16, breaks = 5))

# Add grouping variable to each data set
grp1.ord$group <- "1"
grp2.ord$group <- "2"
grp3.ord$group <- "3"

# Combine data sets
my.data.combined <- rbind(grp1.ord, grp2.ord, grp3.ord)

# Export the data set for use in other projects
write.xlsx(my.data.combined, file = "mydatacombined.xlsx", sheetName = "data", append = FALSE)
