# Load rddtools package
library("rddtools")
library("wooldridge")
library("rdrobust")
# Use data from Lee (2008)
data("house")
# Set outcome, forcing and cutoff variable
plot(house)
house_rdd <- rdd_data(y=y, x=x, cutpoint= 0, data=house)
plot(house_rdd)
# Estimate RDD
reg_para <- rdd_reg_lm(rdd_object=house_rdd)
# Print results
print(reg_para)

plot(reg_para)


# Restrict sample to bandwidth area
bw_ik <- rdd_bw_ik(house_rdd)
reg_para_ik <- rdd_reg_lm(rdd_object=house_rdd, bw=bw_ik)
reg_para_ik



# Estimate effect
rdrobust(house$y, house$x)
rdplot(house$y, house$x)
