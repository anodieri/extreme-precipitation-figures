################################################
## Plot correlations between return levels and
## elevation
################################################

# load libraries ----------------------------------------------------------

library(ggplot2) #to plot
library(reshape2) #to convert dataset to long format


# load data ---------------------------------------------------------------


#PATH: TO EDIT IF NEEDED
load("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/elevation_correlation/CORR_DDF_MOD_BE.RData")



# prepare data ------------------------------------------------------------


#convert this to a dataframe
cors<-data.frame(CORR_DDF_MOD_OBS_BE)
colnames(cors)<-c('run','1 h','24 h') #set column names

#create column that shows if row belongs to EURO-CORDEX ensemble or CORDEX.be
cors$euro<-grepl('EURO-CORDEX', cors$run, fixed = TRUE)

#convert to long format: this is easier to use in ggplot
cors<-melt(cors,id=c('euro','run'))
names(cors)[names(cors) == 'variable'] <- 'dur' #set col names
names(cors)[names(cors) == 'value'] <- 'cor'


# plotting -------------------------------------

#some hacking to be able to get legends in plot
cors$shape<-'star' 
cors$fill<-'star'


#plotting
g<-ggplot()+
  #boxplots for the EURO-CORDEX models:
  geom_boxplot(cors[cors$euro==TRUE,],mapping=aes(y=cor,x=dur, fill = fill), 
               color='gray45')+
  #points for the CORDEX.be models
  geom_point(cors[cors$euro==FALSE,], mapping=aes(y=cor,x=dur, shape = shape),
             size=2, color='black')+
  scale_shape_manual('', labels=c('CORDEX.be'), values=c(15))+ #to get legend for obs
  scale_fill_manual('',labels=c('EURO-\nCORDEX'), values = 'lightseagreen')+ #set color palette for the boxplot
  ylab('Correlation')+
  xlab('Duration')+
  #ylim(c(-0.6,1))+ #set limits 
  theme(text=element_text(size=12), #adjust text size and so on
        legend.position = "bottom", #put legend on the bottom
        legend.key.size = unit(0.7, 'cm')) #set size of the keys in the legends


g

#save the plot
#PATH: EDIT IF NEEDED
ggsave("C:/Users/Anouk/Documents/cursussen/MP/extreme precipitation figures/elevation_correlation/presaved figures/eurocordex_cordex_be.pdf",
       g, width=5, height = 5, units = 'in', dpi = 300)  

