suffixes = c("a","b","c","d","e")
for(i in 1:length(dupes$trkSub)){
    old_ID = dupes$trkSub[i]
    freq = dupes$Freq[i]

    locations = which(PSV$trkSub == old_ID, arr.ind=TRUE)
    k=1
    for(j in seq(1,freq,3)){
        suff = suffixes[k]
        PSV$trkSub[locations[j]] = paste0(old_ID,suff)
        PSV$trkSub[locations[j+1]] = paste0(old_ID,suff)
        PSV$trkSub[locations[j+2]] = paste0(old_ID,suff)
        k = k+1
    }
}

asteroid_params = na.omit(asteroid_params)
MPCORB = na.omit(MPCORB)
density1 = density(asteroid_params$i)
density2 = density(MPCORB$i)
plot <- ggplot() 
        + geom_histogram(data=asteroid_params, aes(x=i, y = ..density..), bins=200,fill="blue",alpha=0.6) +
        geom_histogram(data=MPCORB, aes(x=i, y=..density..),bins=200,fill="green",alpha=0.6)+
        scale_y_continuous(sec.axis = sec_axis( ~ . / max(density1$y) * max(density1$y), name = "Density (Normalized)")) +
        labs(title = "Stacked Histograms with Normalized y scale", x = "Inclination") +
        scale_x_continuous(limits=c(0,90))