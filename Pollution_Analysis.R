# https://catalog.data.gov/dataset/clean-air-status-and-trends-network-castnet-ozone
# https://www.epa.gov/castnet/castnet-ozone-monitoring
# https://www.epa.gov/castnet/download-data

# Read in the dataset into the variable called gas
gas <- read.csv("C:/Users/bwc07/Downloads/table_export.csv")

# Create function to filter data set 
filter_vec <- function(vector) {
  # Create new vector that takes in numbers
  new_vector <- numeric()
  # Iterate over the whole column
  for (x in vector) {
    # If item in column is empty, skip
    if (x == '-') {
      next
      # If there is a value, continue
    } else {
      # The value at x, convert it into a double
      temp <- as.double(x)
      # Add to the vector called new_vector
      new_vector <- append(new_vector, temp)
    }
  }
  return(new_vector)
}

# Filtered Calcium data, but there was no 
# connection found
Ca <- filter_vec(gas$Ca)

# Filtered Nitric acid data
HNO3 = filter_vec(gas$HNO3)

# Filtered Potassium Data, but no connections
# could be found
K = filter_vec(gas$K)

# Filtered Magnesium Data, but no connections could be 
# found
Mg = filter_vec(gas$Mg)

#Filtered Sodium Data
Na = filter_vec(gas$Na)

# Filtered Ammonia Data
NH4 = filter_vec(gas$NH4)

# Filtered Nitrate
NO3 = filter_vec(gas$NO3)

# Filtered Sulfur Dioxide
SO2 = filter_vec(gas$SO2)

# Filtered Sulfate
SO4 = filter_vec(gas$SO4)

# Filtered Total Nitrate
TNO3 = filter_vec(gas$TNO3)

#I am have to further filter data sets so that 
# I can make a data frame of 4062 items
Ca <- Ca[-c(4064, 4063)]
HNO3 <- HNO3[-c(4065, 4064, 4063)]
Mg <- Mg[-c(4065, 4064, 4063)]
NH4 <- NH4[-c(4065, 4064, 4063)]
NO3 <- NO3[-c(4065, 4064, 4063)]
SO2 <- SO2[-c(4063)]
SO4 <- SO4[-c(4065, 4064, 4063)]
TNO3 <- TNO3[-c(4065, 4064, 4063)]
df <- data.frame(Ca, HNO3, K, Mg, NH4, NO3, SO2, SO4, TNO3)


# Plotted all histograms of each Gas to find trends of 
# their levels. This helped me pick TNO3, because it was
# well described and there were correlations to other gases
# like HNO3 or NH4 that are well known in chemistry.

# Some Notes on some of the histograms:
# Calcium is very right skewed
# Potassium and Na did not deviate much and has a very
# low average concentration in the atmosphere which is expected
# because normally, it is in the ground as a mineral.
# SO2 looks the most like a Gaussian distribution, but it 
# is still right skewed.
# Overall, every histogram is right skewed with varying degress
# of how much.
hist(Ca, xlim = c(0, 6), breaks = seq(0, 6, 0.1))
hist(HNO3, xlim = c(0, 3), breaks = seq(0, 3, 0.1))
hist(K, xlim = c(0, 6.3), breaks = seq(0, 6.3, 0.01))
hist(Mg, xlim = c(0, 0.9), breaks = seq(0, 1, 0.01))
hist(Na, xlim = c(0, 6.6), breaks = seq(0, 6.6, 0.01))
hist(NH4, xlim = c(0, 7), breaks = seq(0, 7, 0.01))
hist(SO2, xlim = c(0, 4), breaks = seq(0, 4, 0.01))
hist(SO4, xlim = c(0, 5), breaks = seq(0, 5, 0.01))
hist(TNO3, main = "Histogram of Concentration of TNO3 within the Atmosphere", 
     xlab = expression(plain("µg/m"^"3")), xlim = c(0, 6), 
     breaks = seq(0, 7.3, 0.01))

# Where does 95% of the data fall for TNO3?
# Find the range so we know where the min and max are
range(TNO3)
# Find mean of the concentrations
mean(TNO3)
# Find the standard deviation of TNO3 conc.
std_TNO3 <-sd(TNO3)
# Print mean(TNO3) +/- 2*s
print(mean(TNO3) + 2*std_TNO3)
print(mean(TNO3) - 2*std_TNO3)

# I want to find how many values are outside of 
# mean(TNO3) +/- 2*s which is only 177 out of 4062
# which is good because most of the data is still
# within range
out_of_range <- sum(TNO3 < 0 | TNO3 > 2.6612)
out_of_range

# Beginning of Model Building for air pollutant TNO3
#TNO3 (Total reactive Nitrogen Oxides includes various 
# nitrogen oxides such as NO, NO2, and NOx. 

# Nitric acid is a component of acid rain, which can have 
# negative impacts on plants, animals, and ecosystems. 
# Acid rain occurs when sulfate (SO4) and nitrogen 
# oxides (NOx), which are produced by fossil fuel combustion 
# like in transportation, and react with atmospheric 
# moisture to form sulfuric acid and nitric acid.

# I know any NOx should in theory influence
# TNO3 levels since all NOx are produced back
# and forth in nature.
# This model had a bad R^2, but the F-statistic was
# very good so I kept this variable in mind when 
# building the model but I did not use it immediately
lmod1 <- lm(TNO3 ~ HNO3, data=df)
summary(lmod1)

# Similar situation with NO3 where it has a bad
# R^2 but a very high F-statistic so I also kept
# this variable in mind just in case I could use it
lmod2 <- lm(TNO3 ~ NO3, data=df)
summary(lmod2)

# I have a significantly higher R^2 = 0.70 and an incredibly high
# F-statistic for this model so I know for sure that I
# have a great variable to use in the model
lmod3 <- lm(TNO3 ~ NH4, data=df)
summary(lmod3)

# I also chose to model SO4 because 
# to see how it would interact with the data since
# I know SO4 interacts with other pollutants to form
# NOx. It had a very high F-statistic but a low R^2
lmod4 <- lm(TNO3 ~ SO4, data=df)
summary(lmod4)

# After modifying other variables, I find the best fit with the best
# residual analysis and goodness of fit, I get the following:
lmod <- lm(TNO3 ~ NH4 + NO3 + HNO3 + I(SO4^2) + SO4:HNO3, data=df)  ### AMAZING MODEL
# The F-statistic is insanely high which gives me confidence that at 
# least one of the parameters are significant. I then check the R^2
# and find that it fits the data incredibly well. Then, I perform
# two parameter test on NH4 since it has a very high corrlation by itself
# and HNO3:SO4 since it is a known reaction to form NOx, but I was very surprised
# to find such high Pr(>|t|). The most surprising part was when I removed the 
# variables, I did not get the same best model, so when I added each one back at a
# time, I gathered the best model.
summary(lmod)

# This shows a great correlation that shows the error follows a normal
# distribution with slight light tailing at both ends, but this is expected
# especially for any real data set. It will almost never be perfect
plot(lmod, which = 2)

# I also looked at this residual plot, which definitely showed that the
# residuals were leaning more into the the range of 0 to 2, which of course
# should not be the case, but I actually enlarged the plot and looked into the
# residuals that were past the x-axis =3 and I found most of those values were
# outliers so if you remove those values, you get a perfectly random plot of residuals.
plot(lmod, which=1)


# Use of Pearson Coefficient 
# Suggests a strong linear relationship between the levels of
# TNO3 and NH4 with 95% confidence
# Since r=0.83, it can be implied that as TNO3 levels increase,
# so does the levels of NH4
cor.test(TNO3, NH4, method='pearson')

# I wanted to find some relationships between the variables
# There are are very clear linear behaviors of NH4 vs. TNO3, NO3 vs. TNO3, NO3 vs. NH4
options(repr.plot.width = 10, repr.plot.height = 10, repr.plot.res = 300)
pairs(df[,c("TNO3", "HNO3", "NH4", "NO3", "SO4")],
      pch=20, # point shape
      lower.panel = NULL # don't show below diagonal 
)

# Minimum sum of the squared errors. 
SSE = sum((lmod$residuals)^2)     # = 0.001
# Get the degrees of freedom
deg_free = lmod$df
# Root Mean Square aka Residudal Standard Error
s = sqrt(SSE/deg_free)            # = 0.0005

# We expect that the model will roughly predict future 
# levels of TNO3 based on the independent gases to 
# about ±2s = 0.001 ug/m^3 which is a reasonably small concentration
#±2s = ±2(133.5) = ±267 dollars.will provide a rough approximation to the accuracy with which the model will predict future values of y for given values of x. Thus, in Example 4.1, we expect the model to provide predictions of auction price to within about ±2s = ±2(133.5) = ±267 dollars.
2*s


# This shows a clear relationship that as NH4 levels increase, so does TNO3
# What can also be observed is the fact that if the concentration of NH4 is
# within 0-0.5 ug/m^3, the rate at which TNO3 will not be nearly as great
# as when the concentration of NH4 > 1.0 ug/m^3
plot(TNO3 ~ NH4, data=df, main = 'Realtionship between TNO3 and Ammonia')
abline(lm(TNO3~NH4,data=df), col='red', lwd=2)



# TNO3 = 0.0000131SO4^2 + 0.0000424SO4*HNO3 + 0.984HNO3 + NO3 - 5.72NH4
newdata  = data.frame('SO4'=mean(SO4), 'HNO3'=mean(HNO3), 'NO3'=mean(NO3), 'NH4'=mean(NH4))
#forming a confidence interval with alpha=0.05 
predict(lmod, newdata = newdata, interval='confidence', level=0.95)

# forming a prediction interval
predict(lmod, newdata = newdata, interval='prediction', level=0.95)








# Prediction plot, but it looks too messy
plot(TNO3 ~ NH4, data=df)
pred <- predict(lmod)
ix <- sort(NH4, index.return=T)$ix
lines(NH4[ix], pred[ix], col='red', lwd=2)











dens <- density(normalized_TNO3)

# Plot the density as a line
y_max <- max(normalized_TNO3)
y_axis_range <- c(0, y_max * 1.2)  # Adjust the multiplication factor as needed
plot(NULL, xlim = range(normalized_TNO3), ylim = y_axis_range, xlab = "X", ylab = "Density", 
     main = "Histogram with Trend Line")
normalized_TNO3 <- scale(TNO3)
hist(normalized_TNO3)

# Plot the histogram as bars
hist(normalized_TNO3, freq = FALSE, add = TRUE, col = "lightblue")
# Plot the density as a line
lines(TNO3, col = "red", lwd = 3)







# Set the parameters
height <- 40
mean <- 1.05

# Generate random Poisson-distributed data
data <- rpois(4092, mean)

# Create a histogram with more bins and higher resolution x-axis range
hist(data, breaks = seq(0, 50, 0.01), freq = FALSE, main = "Poisson Distribution",
     xlab = "X", ylab = "Density", xlim = c(0, 5), col = "blue")

# Generate a higher resolution x-axis range
x <- seq(0, max(data), length.out = 1000)

# Add a line representing the theoretical probability density function
lines(x, dpois(x, mean), type = "l", lwd = 2, col = "red")


linear_model <- lm(Ca ~ I(K^2) + K + I(Mg^2) + Mg + TNO3, data=df)
summary(linear_model)
plot(linear_model)


f <- 1/log(NH4)
### I'VE BEEN WORKING WITH THIS ONE
temp1 <- lm(log(SO4) ~ f + log(I(NO3^2)) + NH4*NO3*HNO3 + 
              K*Mg*Ca + log(I(Mg^2)), data=df)
summary(temp1)

tt <- lm(log(SO4) ~ I(NO3^2) + log(I(Mg^2)) + log(NH4) + NO3 + log(HNO3) + 
           log(K) + Mg + log(Ca) + NH4:NO3 + NH4:HNO3 + 
           K:Ca + NH4:NO3:HNO3 + one_SO2, data=df)
summary(tt)












######################################################
te <- lm(log(SO4) ~ log(I(Mg^2)) + log(NH4) + log(I(NO3^2)) + 
           NO3 + HNO3 + HNO3:NO3 + log(I(SO2^2)) + one_SO2, data=df)
summary(te)
plot(te)

oN <- 1/log(NH4)
aa <- lm(log(SO4) ~ log(NH4), data=df)
summary(aa)





aa <- lm(log(I(NO3^2)) + NH4 + NO3 + log(I(Mg^2)) + NH4:HNO3 + 
           NH4:HNO3 + NO3:HNO3 + NH4:K + NO3:K + HNO3:K + 
           NH4:Ca + NO3:Ca + Mg:Ca + NH4:NO3:K + NO3:HNO3:K + 
           NH4:NO3:Mg + NH4:HNO3:Mg + NH4:K:Mg + NO3:K:Mg + NH4:K:Ca + 
           NO3:K:Ca + NH4:HNO3:K:Mg + NH4:HNO3:K:Ca + NH4:K:Mg:Ca + 
           NO3:K:Mg:Ca + NH4:HNO3:K:Mg:Ca + NH4:NO3:HNO3:K:Mg:Ca, data=df)
summary(aa)

temp1 <- lm(log(SO4) ~ f + NH4 + NO3 + HNO3 + NH4:NO3 + NH4:NO3:HNO3 + log(I(Mg^2)) + K*Mg + log(I(SO2^2)), data=df)
summary(temp1)

#ALSO REALLY WORKS
V <- lm(log(SO4) ~ f + NH4*NO3*HNO3 + I(NO3^4))
summary(V)



###########################
# OLD DATA
one_SO2 <- 1/SO2
lmod2 <- lm(log(SO4) ~ log(I(TNO3^2)) + log(NH4) + NO3 + HNO3 + one_SO2, data=df)
summary(lmod2)

lmod3 <- lm(SO4 ~ I(TNO3^3) + NH4*NO3 + NO3*HNO3 + Mg, data=df)
summary(lmod3)

###THIS TEMP IS THE BEST MODEL RIGHT NOW
temp <- lm(SO4 ~ I(TNO3^3) + I(NH4^2) + NH4:NO3 + NO3:HNO3 + Mg + HNO3 + NH4, data=df)
summary(temp)
f <- 1/log(NH4)

### I'VE BEEN WORKING WITH THIS ONE
temp1 <- lm(log(SO4) ~ f + log(I(NO3^2)) + NH4*NO3*HNO3 + Mg + NH4*HNO3 + log(I(Mg^2)) + K*Mg*Ca, data=df)
summary(temp1)

t <- lm(log(SO4) ~ I(NH4^2) + I(Mg^2) + Mg + I(NO3^2) + NO3*HNO3*NH4)
summary(t)
plot(t)

a <- lm(log(SO4) ~ I(SO2^4) + I(SO2^3) + I(SO2^2) + SO2)
summary(a)
