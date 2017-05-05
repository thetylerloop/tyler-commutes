library(dplyr)
library(ggplot2)
library(haven)
library(reshape2)
library(survey)

# Load IPUMS data (extract #26 from my account).
# It contains: "Travel time to work, manner of travel, and carpool status for employed residents of Tyler, TX metro area only. (2000, 2005-2015)"
ipums.orig <- read_dta("usa_00026.dta")

# Reduce to contingous years and delete unneeded columns
ipums <- ipums.orig %>%
  filter(year >= 2005) %>%
  select(-met2013, -empstat, -empstatd)

# Convert factors
ipums$year <- as.factor(as.character(ipums$year))

## Recoding ipums$tranwork into ipums$tranwork.factor
ipums$tranwork.factor <- as.character(ipums$tranwork)
ipums$tranwork.factor[ipums$tranwork == "10"] <- "Personal vehicle"
ipums$tranwork.factor[ipums$tranwork == "0"] <- NA
ipums$tranwork.factor[ipums$tranwork == "31"] <- "Bus"
ipums$tranwork.factor[ipums$tranwork == "60"] <- "Other"
ipums$tranwork.factor[ipums$tranwork == "50"] <- "Walked"
ipums$tranwork.factor[ipums$tranwork == "70"] <- "Worked at home"
ipums$tranwork.factor[ipums$tranwork == "40"] <- "Bike"
ipums$tranwork.factor[ipums$tranwork == "20"] <- "Personal vehicle"
ipums$tranwork.factor[ipums$tranwork == "35"] <- "Taxi"
ipums$tranwork.factor[ipums$tranwork == "34"] <- "Train"
ipums$tranwork.factor <- factor(ipums$tranwork.factor)

## Recoding ipums$carpool into ipums$carpool.factor
ipums$carpool.factor <- as.character(ipums$carpool)
ipums$carpool.factor[ipums$carpool == "1"] <- "Drives alone"
ipums$carpool.factor[ipums$carpool == "2"] <- "Carpools"
ipums$carpool.factor[ipums$carpool == "0"] <- NA
ipums$carpool.factor <- factor(ipums$carpool.factor)

## Recoding ipums$year into ipums$year_group
ipums$year_group <- as.character(ipums$year)
ipums$year_group[ipums$year == "2005"] <- NA
ipums$year_group[ipums$year == "2006"] <- NA
ipums$year_group[ipums$year == "2007"] <- "2007-2009"
ipums$year_group[ipums$year == "2008"] <- "2007-2009"
ipums$year_group[ipums$year == "2009"] <- "2007-2009"
ipums$year_group[ipums$year == "2010"] <- "2010-2012"
ipums$year_group[ipums$year == "2011"] <- "2010-2012"
ipums$year_group[ipums$year == "2012"] <- "2010-2012"
ipums$year_group[ipums$year == "2013"] <- "2013-2015"
ipums$year_group[ipums$year == "2014"] <- "2013-2015"
ipums$year_group[ipums$year == "2015"] <- "2013-2015"
ipums$year_group <- factor(ipums$year_group)

# Add a column of ones for weighted counts
ipums$one <- 1

# Survey design for single-year estimates
design <- svydesign(
  ids = ~ 1,
  data = ipums,
  weights = ~ perwt
)

# Count observations by year
ipums.counts <- svyby(~ one, ~ year, design, svytotal, vartype = "ci")

# Compute mean transit time
ipums.means <- svyby(~ trantime, ~ year, design, svymean, vartype = c("se", "ci"))

# Compute median transit times
ipums.medians <- svyby(~ trantime, ~ year, design, svyquantile, quantiles=0.5, ci=TRUE)

# Test for statistical significance
SigTest <- function(aMean, bMean, aSE, bSE, crit = 1.645) {
  abs((aMean - bMean) / sqrt((aSE ^ 2) + (bSE ^ 2))) > crit
}

# Test 1y change
y1 <- ipums.means["2005",]
y2 <- ipums.means["2015",]

SigTest(y1$trantime, y2$trantime, y1$se, y2$se, crit = 1.645)

ggplot(data = ipums.means, aes(x = year, y = trantime, group = 1)) +
  geom_line(size = 2) + 
  geom_ribbon(aes(ymin = ci_l, ymax = ci_u), alpha = 0.5)

# Work-from-homers
wfh <- ipums %>%
  filter(tranwork.factor == "Worked at home")

wfh.design <- svydesign(
  ids = ~ 1,
  data = wfh,
  weights = ~ perwt
)

wfh.confint <- confint(svyby(~ one, ~ year, wfh.design, svytotal))

# 3-year pooled data
pooled <- ipums %>%
  filter(!is.na(year_group))

design_3y <- svydesign(
  ids = ~ 1,
  data = pooled,
  weights = ~ perwt
)

# Count observations by year
pooled.counts <- svyby(~ one, ~ year_group, design_3y, svytotal, vartype = "ci")

# Compute mean transit time
pooled.means <- svyby(~ trantime, ~ year_group, design_3y, svymean, vartype = c("se", "ci"))

# Verify significance of 3y change
y1 <- pooled.means["2007-2009",]
y2 <- pooled.means["2013-2015",]

SigTest(y1$trantime, y2$trantime, y1$se, y2$se, crit = 1.645)

ggplot(data = pooled.means, aes(x = year_group, y = trantime, group = 1)) +
  geom_line(size = 2) + 
  geom_ribbon(aes(ymin = ci_l, ymax = ci_u), alpha = 0.5)

# Compare to counties w/ approx same population as Smith?