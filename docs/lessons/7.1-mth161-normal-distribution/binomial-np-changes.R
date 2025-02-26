# load packages
library(tidyverse)
library(openintro)
library(gganimate)
library(latex2exp)
data(COL)
seed <- 111

# set hyperparameters
set.seed(seed) # for reproducibility

# set parameter and PMF of the Bernoulli r.v
n <- 15
x <- seq(0,n,1) # set outcome from 0 to n
N <- 100 # number of transitions
p <- seq(0.3,0.8,by=1/N) # probabilities of "success" as a vector

# generate the Binomial PMF while tracking p
df <- tibble(p=c(),x=c(),pmf=c())
for (i in p){
  pmf_i <- dbinom(x,n,i)
  tibble_i <- tibble(p=rep(i,length(pmf_i)),x=x,pmf=pmf_i)
  df <- df %>% rbind(tibble_i)
}

# plot Binomial PMFs
p1 <- ggplot(df,aes(x=x,y=pmf)) + 
  geom_point(size=3,color="black") + # size here is defined for all points
  geom_segment(aes(x=x,xend=x, # draws a line between two defined points
                   y=rep(0,length(x)),yend=pmf), 
               color="black") + 
  geom_vline(xintercept = n*p, color="red", linewidth=1) + 
  geom_text(aes(n*p,0),label = "Expectation", 
            vjust = -15, hjust=-0.25,color="red") + 
  ggtitle(paste("Binomial PMF (n=",n,", p={closest_state})",sep="")) + # sets the title of the plot
  ylim(0,0.25) + 
  theme_minimal() # set theme of entire plot

# animation options
p1_anim <- p1 + 
  transition_states(p)

# animate and save as .gif
animate(p1_anim, nframes = round(length(p)/1), 
        renderer = gifski_renderer(paste("binomial-",n,"p-changes.gif",sep="")),
        height=1000,width=2000,res=300)
