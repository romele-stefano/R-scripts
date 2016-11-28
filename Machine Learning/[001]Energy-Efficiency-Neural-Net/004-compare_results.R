par(mfrow=c(2,2))

# Plot Neural Net
plot(compare$heat_load,compare$heat_load_predicted,col='red',main='Heating Load, Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend="NN_heat",pch=18,col='red', bty='n')

plot(compare$cool_load,compare$cool_load_predicted,col='blue',main='Cooling Load, Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend="NN_cool",pch=18,col='blue', bty='n')


# Plot Generalized Linear Model
plot(compare_lm$heat_load,compare_lm$heat_load_predicted,col='red',main='Heating Load, Real vs predicted GLM',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend="GLM_heat",pch=18,col='red', bty='n')

plot(compare_lm$cool_load,compare_lm$cool_load_predicted,col='blue',main='Cooling Load, Real vs predicted GLM',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend="GLM_cool",pch=18,col='blue', bty='n')

