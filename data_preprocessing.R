#set working directory
setwd("~/big_data/LAB")

#load library for readind edf data format
library(edfReader)

#get the headers of the signals
SignlHeaders <- edfReader::readEdfHeader('~/big_data/LAB/Paziente1/chb01_20.edf')

#read in the signals by headers
Signals <- edfReader::readEdfSignals(SignlHeaders)

#extract first batch of signal
signal_fp1_f7 = Signals$`FP1-F7`$signal

# min max normalization function
normalize <- function(x)
{
  return(x - min(x)*1.01) / (max(x) * 1.01 - min(x) * 1.01)
}

normalized_signal = normalize(signal_fp1_f7)

#import scales library to rescale data
library(scales)

#scale signal to min = 0 and max=1
scaled_signal = rescale(normalized_signal)

#plot scaled data to see distribution
library(fitdistrplus)
fitdistrplus::plotdist(scaled_signal)

#split signal in to segment with a widow size of 1000
#segment size of 682
signal_segments = split(scaled_signal,  ceiling(seq_along(scaled_signal)/1000))

#extract features into a dataframe
#features choosen are mean,meadian , std devation, variance and entropy
library(pracma)

features_dataframe = data.frame(mean=double(),
                                median=double(),
                                variance=double(),
                                std=double(),
                                entropy=double())

#build feature data frame
for(i in signal_segments){
  features = data.frame(mean=mean(i),
                        median=median(i),
                        variance=var(i),
                        std=sd(i),
                        entropy=approx_entropy(i))
  
  features_dataframe = rbind(features_dataframe,features)
}

write.csv(features_dataframe, file = "features.csv",row.names=FALSE)

