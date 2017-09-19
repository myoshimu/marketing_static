# Define Adstock Rate
#あってるかあとで確認
adstock_rate = 0.50

# Create Data
d<-read.csv("toyota.csv")
advertising <- d$grp
# Calculate Advertising Adstock
# Credit: http://stackoverflow.com/questions/14372880/simple-examples-of-filter-function-recursive-option-specifically
adstocked_advertising = filter(x=advertising, filter=adstock_rate, method="recursive")

# Alternative Method Using Loops Proposed by Linh Tran
adstocked_advertising = numeric(length(advertising))
adstocked_advertising[1] = advertising[1]
for(i in 2:length(advertising)){
  adstocked_advertising[i] = advertising[i] + adstock_rate * adstocked_advertising[i-1]
}

# Graph Data
plot(seq(1,length(advertising)), advertising, type="h", 
     xlab="Time (Usually in Weeks)", ylab="Advertising", 
     ylim=c(0, max(c(advertising, adstocked_advertising))), 
     frame.plot=FALSE)
lines(adstocked_advertising)