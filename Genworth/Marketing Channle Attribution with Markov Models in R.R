library(ChannelAttribution)
# ChannelAttribution, an R library, builds the Markov models that allow us to calculate the 
# number of conversions and/or conversion value that can be attributed to each marketing channel. 
# In other words, ChannelAttribution uses Markov models to determine each channel's contribution to conversion and/or value.

# Removal effect: the probability of converting when a step is completely removed

library(reshape)
library(ggplot2)

# Read in data
PathData = read.csv("../Demo.csv")
str(PathData)
dim(PathData)

# Path Variable - Teh steps a user takes across sessions to comprise the sequences
# Conversion Variable - How many times a user converted
# Value Variable - The monetary value of each marketing channel
# NULL Variable - How many tiems a user exited

# Build the simple heuristic models (First Click / Last Click / Linear Attribution)
H <- heuristic_models(PathData,"path","total_conversions",var_value = "total_conversion_value")
H
# Markov model
M <- markov_model(PathData,"path","total_conversions",var_value="total_conversion_value",order = 1)
M

# Merge the three models with Markov model
R = merge(H,M,by="channel_name")
R1 = R[,colnames(R) %in% c("channel_name","first_touch_conversions","last_touch_conversions","linear_touch_conversions","total_conversion")]
colnames(R1) = c("channel_name","first_touch","last_touch","linear_touch","markov_model")
R1 = melt(R1,id="channel_name")

# Plot the total conversions
ggplot(R1, aes(channel_name, value, fill = variable)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('TOTAL CONVERSIONS') + 
  theme(axis.title.x = element_text(vjust = -2)) +
  theme(axis.title.y = element_text(vjust = +2)) +
  theme(title = element_text(size = 16)) +
  theme(plot.title=element_text(size = 20)) +
  ylab("")

# Create the Total Value Bar
R2 = R[,colnames(R) %in% c("channel_name","first_touch_value","last_touch_value","linear_touch_value","total_conversion_value")]
colnames(R2) = c("channel_name","first_touch","last_touch","linear_touch","markov_model")
R2 = melt(R2,id="channel_name")

# Plot the total conversions
ggplot(R2, aes(channel_name, value, fill = variable)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('TOTAL VALUE') + 
  theme(axis.title.x = element_text(vjust = -2)) +
  theme(axis.title.y = element_text(vjust = +2)) +
  theme(title = element_text(size = 16)) +
  theme(plot.title=element_text(size = 20)) +
  ylab("")




















