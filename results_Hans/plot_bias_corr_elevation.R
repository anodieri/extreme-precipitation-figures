################################################
## Plot the bias and correlations between 
## return levels and elevation for EUROCORDEX
## and CORDEX.be
################################################

# load libraries ----------------------------------------------------------

library(ggplot2) #to plot
library(reshape2) #to convert dataset to long format
library(cowplot) #to put figures in a grid

# load data ---------------------------------------------------------------


#PATH: TO EDIT IF NEEDED
load("C:/Users/dierickx/OneDrive - vki.ac.be/Documents/master/MP/extreme precipitation figures/results_Hans/CORR.RData")
load("C:/Users/dierickx/OneDrive - vki.ac.be/Documents/master/MP/extreme precipitation figures/results_Hans/BIAS.RData")



# prepare data bias  ------------------------------------------------------------


#convert this to a dataframe for the bias
bias<-data.frame(BIAS)
colnames(bias)<-c('run','1 h','2 h', '3 h','6 h', '12 h','24 h') #set column names

#create column that shows if row belongs to EURO-CORDEX ensemble or CORDEX.be
bias$euro<-grepl('EURO-CORDEX', bias$run, fixed = TRUE)

#convert to long format: this is easier to use in ggplot
bias<-melt(bias,id=c('euro','run'))
names(bias)[names(bias) == 'variable'] <- 'dur' #set col names
names(bias)[names(bias) == 'value'] <- 'bias'

# prepare data correlations  ------------------------------------------------------------

#extract the observations
obs<-data.frame(CORR$CORRH_OBS)
colnames(obs) ='cor'
obs$dur <- c('1 h','2 h', '3 h','6 h', '12 h','24 h') #set durations

#remove the observations from the original data structure (has different number of columns)
CORR$CORRH_OBS<-NULL

#convert this to a dataframe for the cors
cors<-data.frame(CORR)
colnames(cors)<-c('run','1 h','2 h', '3 h','6 h', '12 h','24 h') #set column names

#create column that shows if row belongs to EURO-CORDEX ensemble or CORDEX.be
cors$euro<-grepl('EURO-CORDEX', cors$run, fixed = TRUE)

#convert to long format: this is easier to use in ggplot
cors<-melt(cors,id=c('euro','run'))
names(cors)[names(cors) == 'variable'] <- 'dur' #set col names
names(cors)[names(cors) == 'value'] <- 'cor'


# plotting the bias boxplots-------------------------------------

#set colors for plotting
obs_col = 'black'
be_col = '#05598c'
euro_col = 'lightseagreen' #'darkseagreen2' #'#fefeb2'#
euro_col_lines = 'gray45'

#some hacking to be able to get legends in plot
bias$shape<-'star' 
bias$fill<-'star'


#plotting
g<-ggplot()+
  #boxplots for the EURO-CORDEX models:
  geom_boxplot(bias[bias$euro==TRUE,],mapping=aes(y=bias,x=dur, fill = fill), alpha=0.5,
               color=euro_col_lines)+
  #points for the CORDEX.be models
  geom_point(bias[bias$euro==FALSE,], mapping=aes(y=bias,x=dur, shape = shape),
             size=2.5, color=be_col)+
  scale_shape_manual('', labels=c('CORDEX.be'), values=c(15))+ #to get legend for obs
  scale_fill_manual('',labels=c('EURO-\nCORDEX'), values = euro_col)+ #set color palette for the boxplot
  ylab('Bias (%)')+
  xlab('Duration')+
  geom_hline(yintercept=0, linetype ='dashed')+
  #ylim(c(-0.6,1))+ #set limits 
  theme(text=element_text(size=14), #adjust text size and so on
        legend.position = "bottom", #put legend on the bottom
        legend.key.size = unit(0.7, 'cm')) #set size of the keys in the legends


g


# plotting the correlations boxplots-------------------------------------

#some hacking to be able to get legends in plot
cors$shape<-'star' 
cors$fill<-'star'
obs$shape<-'circle'

#plotting
h<-ggplot()+
  #boxplots for the EURO-CORDEX models:
  geom_boxplot(cors[cors$euro==TRUE,],mapping=aes(y=cor,x=dur, fill = fill), alpha = 0.5,
               color=euro_col_lines)+
  #points for the CORDEX.be models
  geom_point(cors[cors$euro==FALSE,], mapping=aes(y=cor,x=dur, shape = shape),
             size=2.5, color=be_col)+
  geom_point(obs, mapping=aes(y=cor,x=dur, shape = shape),
             size=2.5, color=obs_col)+
  scale_shape_manual('', labels=c('Observations','CORDEX.be'), values=c(16, 15))+ #to get legend for obs
  scale_fill_manual('',labels=c('EURO-\nCORDEX'), values = euro_col)+ #set color palette for the boxplot
  ylab('Correlation with elevation')+
  xlab('Duration')+
  geom_hline(yintercept=0, linetype ='dashed')+
  #ylim(c(-0.6,1))+ #set limits 
  theme(text=element_text(size=14), #adjust text size and so on
        legend.position = "bottom", #put legend on the bottom
        legend.key.size = unit(0.7, 'cm')) #set size of the keys in the legends


h

# Plotting both bias and correlations -------------------------------------

grid<-plot_grid(g, h, labels = c('a)','b)'), label_fontface = 'plain')

#save the plot
#PATH: EDIT IF NEEDED
ggsave("C:/Users/dierickx/OneDrive - vki.ac.be/Documents/master/MP/extreme precipitation figures/results_Hans/bias_cors.pdf",
       grid, width=11, height = 5, units = 'in', dpi = 300)  


