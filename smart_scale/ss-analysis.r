# Note - this uses the restriktor package for constrained regression.
#      Available at: http://www.restriktor.ugent.be/

library('restriktor') # Used for the constrained regression.


data <- read.csv('ss-data.csv')

# --------------- Part 1: Data Preprocessing --------------------------
recode <- function(vec, from.vals, to.vals) {
	f = function(v) {
		for(i in 1:length(from.vals)) {
			if(v == from.vals[i]) { return(to.vals[i]) }
		}
		return(v)
	}
	return(sapply(as.vector(vec), f))
}

# Function for imputing mean of a vector to its missing values.
impute.mean <- function(vec) {
	vec[is.na(vec)] = mean(vec, na.rm=TRUE)
	vec
}

# Function for imputing value of 0 to each missing value in the vector.
impute.0 <- function(vec) {
	vec[is.na(vec)] = 0
	vec
}
# Clean and recode numeric columns.
numeric.columns <- colnames(data)[8:ncol(data)]
for(col in numeric.columns) {
	data[[col]] <- as.numeric(as.character(data[[col]]))
}

# Impute 0 to all missing Land.Use.Score values, since area types C and D don't use it.
data[[20]] <- imputer.f(data[[20]])

imputer.f <- impute.0 # Missing value imputation function - can change if needed

# Impute missing values to component scores and print out the percent missing in each column.
for(i in 8:19) {
	message(paste('Percent Missing Values for Column "', colnames(data)[i], '": ', round(100*(1 - (length(sort(data[[i]]))/nrow(data))), 2)))
	data[[i]] <- imputer.f(data[[i]])
}

# Properly binarize binary columns.
data$Statewide.High.Priority <- as.numeric(sapply(data$Statewide.High.Priority, function(x){if(x == 'x') return(1); return(0);}))
data$District.Grant <- as.numeric(sapply(data$District.Grant, function(x){if(x == 'x') return(1); return(0);}))

# --------------- Part 2: Data Exploration --------------------------

# Explore distributions of different projects.
table(data$Area.Type)
table(data$District)

ss.mean <- mean(data$SMART.SCALE.Score, na.rm=TRUE)
ss.sd <- sd(data$SMART.SCALE.Score, na.rm=TRUE)

aggregate(data$SMART.SCALE.Score ~ data$Area.Type, FUN=mean)
aggregate(data$SMART.SCALE.Score ~ data$District, FUN=mean)


# Look at project-based score data. 
pscore.data <- data[21:ncol(data)]
nrow(na.omit(pscore.data)) / nrow(pscore.data)  # Percent of rows without any missing project-based score data
cor(na.omit(pscore.data)) # Look at correlations that exist among project-based score measures

plot(1:length(sort(data$SMART.SCALE.Score)), sort(data$SMART.SCALE.Score), col='blue', pch=16) # See shape of SSS curve
plot(1:length(sort(data$SMART.SCALE.Score)), log(sort(data$SMART.SCALE.Score)), col='blue', pch=16) # See with log transform 

# SSS by benefit ranks.
plot(data$Benefit.Rank, data$SMART.SCALE.Score, col='blue', pch=16) 
plot(data$State.Rank, data$SMART.SCALE.Score, col='blue', pch=16) 
plot(data$District.Rank, data$SMART.SCALE.Score, col='blue', pch=16) 

# Look at plots - cut off at 100 to exclude the outliers
boxplot(SMART.SCALE.Score~Area.Type,data=data, main="Car Milage Data",
        xlab="Area Type", ylab="Smart Scale Score", ylim=c(0, 50), col="yellow") 

boxplot(SMART.SCALE.Score~District,data=data, main="Car Milage Data",
        xlab="Area Type", ylab="Smart Scale Score", ylim=c(0, 50), col="yellow") 

# --------------- Part 3: Investigating Smart Scale Calculation --------------------------
# Look at Smart Scale Score fit by area type.
# In this section we use regression to calculate the actual weights of each factor by area type. These
# can be then compared to the stated weights in the technical guide to see if they are in fact consistent.

# Step 1: Normalize Measures 
measure.cols <- c('Throughput.Score', 'Delay.Score',
                  'Econ.Dev.Support.Score', 'Intermodal.Access.Score', 'Travel.Time.Reliability.Score',
                  'Access.to.Jobs', 'Disadvantaged.Access.to.Jobs', 'Multimodal.Access.Score',
                  'Crash.Frequency.Score', 'Crash.Rate.Score',
                  'Air.Quality.Score', 'Enviro.Impact.Score')
for(i in measure.cols) {data[[i]] <- 100 * data[[i]] / max(data[[i]])}

# For a given data frame, a set of weights, and a set of selected columns, this function returns a new
# vector corresponding to the weighted averages of the columns.
weighted.column.average <- function(data.frame, weight.vec, selected.columns) {
	v <- 0 
	for(i in 1:length(weight.vec)) {
		v <- v + (weight.vec[i] * data.frame[[selected.columns[i]]])
	}
	v
}

# Calculate the composite factor scores (except Land Use, which is APPARENTLY already given in the data) from 
# component scores according to the Smart Scale November 2017 technical guide, p. 40.
data$Congestion.Score <- weighted.column.average(data, c(0.5, 0.5), c('Throughput.Score', 'Delay.Score'))
data$Economic.Score <- weighted.column.average(data, c(0.6, 0.2, 0.2), c('Econ.Dev.Support.Score', 'Intermodal.Access.Score', 'Travel.Time.Reliability.Score'))
data$Accessibility.Score <- weighted.column.average(data, c(0.6, 0.2, 0.2), c('Access.to.Jobs', 'Disadvantaged.Access.to.Jobs', 'Multimodal.Access.Score'))
data$Safety.Score <- weighted.column.average(data, c(0.5, 0.5), c('Crash.Frequency.Score', 'Crash.Rate.Score'))
data$Environmental.Score <- weighted.column.average(data, c(0.5, 0.5), c('Air.Quality.Score', 'Enviro.Impact.Score'))
# Land use score already in data - no component scores listed.

# Given a dataframe with calculated factor scores, compute the smart scale score according to 
# Table 4.2, p.36 of the technical guide
calc.ss.score <- function(dataset) {
  factor.cols.ab <- c('Congestion.Score', 'Economic.Score', 'Accessibility.Score', 'Safety.Score',
                       'Environmental.Score', 'Land.Use.Score')
  factor.cols.cd <- c('Congestion.Score', 'Economic.Score', 'Accessibility.Score', 'Safety.Score',
                      'Environmental.Score')
  sa <- weighted.column.average(dataset, c(0.45, 0.5, 0.15, 0.5, 0.1, 0.2), factor.cols.ab)
  sb <- weighted.column.average(dataset, c(0.45, 0.5, 0.15, 0.5, 0.1, 0.2), factor.cols.ab)
  sc <- weighted.column.average(dataset, c(0.45, 0.5, 0.15, 0.5, 0.1, 0.2), factor.cols.cd)
  sd <- weighted.column.average(dataset, c(0.45, 0.5, 0.15, 0.5, 0.1, 0.2), factor.cols.cd)
  scores <- c()
  for(i in 1:nrow(dataset)) {
    if(dataset[i, 'Area.Type'] == 'A') {scores[i] <- sa[i]}
    else if(dataset[i, 'Area.Type'] == 'B') {scores[i] <- sb[i]}
    else if(dataset[i, 'Area.Type'] == 'C') {scores[i] <- sc[i]}
    else {scores[i] <- sd[i]}
  }
  scores
}

# Safety.Score > 0  ### --- Left out because it is the only one that results in correct results in Areas B & C.


# This function uses constrained regression to reconstruct the weighting coefficients for a set of SS projects. Returns the fitted constrained model.
ss.reconstruction <- function(dataset, is.ab=TRUE) {
  
  # Set the linear regression equation form
  reg.form <- SMART.SCALE.Score ~ Safety.Score + Congestion.Score + Accessibility.Score + Environmental.Score + Economic.Score + Land.Use.Score + 0
  
  # Build a constrained regression model to find the best fit given that 1) 0 <= each variable <= 1, and 2) sum(variables) = 1
  constraints <- 'Safety.Score > 0
				          Congestion.Score > 0
                  Accessibility.Score > 0
				          Environmental.Score > 0
				          Economic.Score > 0
                  Safety.Score + Congestion.Score + Accessibility.Score + Environmental.Score + Economic.Score + Land.Use.Score + 0 == 1'	
  if(!is.ab) {
    # Set the linear regression equation form without land use
    reg.form <- SMART.SCALE.Score ~ Safety.Score + Congestion.Score + Accessibility.Score + Environmental.Score + Economic.Score + 0
    
    # Build a constrained regression model to find the best fit given that 1) 0 <= each variable <= 1, and 2) sum(variables) = 1
    constraints <- '
				            Congestion.Score > 0
                    Accessibility.Score > 0
                    Environmental.Score > 0
				            Economic.Score > 0
                    Safety.Score + Congestion.Score + Accessibility.Score + Environmental.Score + Economic.Score + 0 == 1'	
  }
  
  fit <- lm(reg.form, data=dataset)
  restriktor(fit, constraints = constraints)
}


# Split out data sets by area type.
ta <- subset(data, Area.Type == 'A')
tb <- subset(data, Area.Type == 'B')
tc <- subset(data, Area.Type == 'C')
td <- subset(data, Area.Type == 'D')


rs.a <- ss.reconstruction(ta, TRUE)
summary(rs.a)

rs.b <- ss.reconstruction(tb, TRUE)
summary(rs.b)

rs.c <- ss.reconstruction(tc, FALSE)
summary(rs.c)

rs.d <- ss.reconstruction(td, FALSE)
summary(rs.d)



# ------------- SCRAPS ----------------------------------
reg.form <- SMART.SCALE.Score ~ Safety.Score + Congestion.Score + Accessibility.Score + Environmental.Score + Economic.Score + Land.Use.Score + 0

f2 <- Land.Use.Score ~ Congestion.Score #Safety.Score# + Congestion.Score + Accessibility.Score + Environmental.Score + Economic.Score

cst.c <- 'Congestion.Score > 0'	
summary(lm(reg.form, data=tc))
summary(lm(f2, data=tc))
rs.c <- ss.reconstruction(tc, constraints=cst.c)
summary(rs.c)

cor(tc[c('Safety.Score', 'Congestion.Score', 'Accessibility.Score', 'Environmental.Score', 'Economic.Score', 'Land.Use.Score')])




rs.d <- ss.reconstruction(td, constraints=cst.c)
summary(lm(reg.form, data=td))
summary(rs.d)



