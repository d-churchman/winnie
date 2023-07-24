library(stringr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggridges)

# Read in the Data from Github
winnie_url = 'https://raw.githubusercontent.com/Priya22/project-dialogism-novel-corpus/master/data/WinnieThePooh/quotation_info.csv'
winnie <- read.csv(winnie_url)
winnie <- winnie %>% select(c(quoteID,speaker, quoteText, quoteByteSpans))

head(winnie)
#collapse Christopher Robin Internal vs. External Speaker
#winnie <- winnie %>% 
#  filter(grepl('Christopher Robin', speaker)) %>% 
#  mutate(speaker='Christopher Robin')

# Clean up speaker names, using string replacement
strings_to_blank <- c(' - External', ' - Story')
for(x in strings_to_blank){
  winnie <- winnie %>% mutate_all(list(~str_replace(., x, '')))}

winnie <- winnie %>% 
  mutate_all(list(~str_replace(., 'Winnie-the-pooh', 'Winnie-the-Pooh')))

winnie <- winnie %>% 
  mutate_all(list(~str_replace(., '_narr', 'The Narrator')))

# Clean up quoteTExt
strings_to_space <- c('\\n', '  ','_')
for(x in strings_to_space){
  winnie <- winnie %>% mutate_all(list(~str_replace(., x, ' ')))}



# Extract the starting byte of each quote from this form '[[750, 777]]'
winnie <- winnie %>% separate(quoteByteSpans,c('startingByte'),', ')
winnie$startingByte <- as.numeric(gsub('\\D','',winnie$startingByte))

# Get the quote length
winnie$quoteLength <- nchar(winnie$quoteText)

# Switch quoteLength to log for better distribution
winnie$quoteLength <- log(winnie$quoteLength, 2)

# Save the Heffalump
heffalump <- filter(winnie, speaker == 'Heffalump')



# Pick which characters to viz
winnie <- filter(winnie, speaker %in% 
                   c(#'The Narrator', 
                     #'Winnie-the-Pooh',
                     'Christopher Robin', 
                     'Eeyore', 
                     'Rabbit',
                     'Piglet'
                     #'Heffalump'
                     #'Kanga', 
                     #'Baby Roo'
                     #'Owl'
                     ))

speaker_count <- length(unique(winnie$speaker))


########################################################
# Cleaning done
# Start Viz
########################################################

# Calculate densities for Starting Byte
pos_dens <- group_by(winnie, speaker) %>%
  do(ggplot2:::compute_density(.$startingByte, NULL)) %>%
  rename(startingByte = x)

# Calculate densities for Quote Length
len_dens <- group_by(winnie, speaker) %>%
  do(ggplot2:::compute_density(.$quoteLength, NULL)) %>%
  rename(quoteLength = x)

# Calc limits for density curves
dens_limit_pos <- max(pos_dens$density) * 1.06
dens_limit_len <- max(len_dens$density) * 1.05 # upper limit of density curves

# get the maximum values
max_dens_pos <- filter(pos_dens, density == max(density)) %>%
  ungroup() %>%
  mutate(
        hjust = rep(0,speaker_count), # horizontal justification 0 left 1 right
        vjust = rep(0,speaker_count),   # vertical just 
        nudge_x = rep(0,speaker_count),
        nudge_y =rep(0,speaker_count),
        label = paste0(speaker)
  )

max_dens_len <- filter(len_dens, density == max(density)) %>%
  ungroup() %>%
  mutate(
    hjust = rep(0,speaker_count), # horizontal justification 0 left 1 right
    vjust = rep(0,speaker_count),   # vertical just 
    nudge_x = rep(0,speaker_count),
    nudge_y =rep(0,speaker_count),
    label = paste0(speaker)
  )

# Breaks variables
breaks = unique(winnie$speaker)
labels = breaks

colors = c('Winnie-the-Pooh'='#E8A12A', 
           'Piglet'='#F1BDC4', 
           'The Narrator'='#afafaf',
           'Christopher Robin'='#7CC3EA',
           'Rabbit'='#E5D179',
           'Eeyore'='#4E545D',
           'Owl'='#987A65',
           'Kanga'='#C8673B',
           'Baby Roo'='#D3965E',
           'Heffalump'='#B8ABDB')

shapes = c('Winnie-the-Pooh'=15, 
           'Piglet'=21, 
           'The Narrator'=17,
           'Christopher Robin'=22,
           'Rabbit'=23,
           'Eeyore'=24,
           'Owl'=25,
           'Kanga'=16,
           'Baby Roo'=17,
           'Heffalump'=8)
alpha_d = 0.05
alpha = 0.2
# Build the scatter plot
scatter <- ggplot(
  winnie, aes(x = startingByte, 
              y = quoteLength, 
              shape = speaker, 
              fill = speaker, color = speaker)
) +     
  # Change the shapes manually
  scale_shape_manual(
    values = shapes,
    breaks = breaks,
    labels = labels,
    name = NULL
  ) +
  scale_color_manual(
    values = colors,
    breaks = breaks,
    labels = labels,
    name = NULL
  ) +
  scale_fill_manual(
    values = alpha(colors,alpha),
    breaks = breaks,
    labels = labels,
    name = NULL
  ) +
  scale_x_continuous(
    name = "Position in Book",
    labels=NULL
  ) +
  scale_y_continuous(
    name = "Log Length",
    labels=NULL
  ) +
  geom_point(
    size=2., 
    stroke = 0.25
  ) + 
  geom_density2d(
    bins = 3,
    linewidth=.4,
    alpha=.6
  ) +
  geom_point(
    size=2.25, 
    stroke = 0.25
  ) + 
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.ticks=element_blank(),
    text=element_text(family='Gill Sans',size=10),
    #panel.border = element_rect(color='#e8e8e8', fill=NA),
    axis.line = element_line(color='#e8e8e8',size=0.5),
    plot.title = element_text(face='bold', size=12)
    ) +
  geom_point(data=heffalump, aes(x = startingByte, 
                              y = quoteLength, 
                              shape = speaker, 
                              #fill = speaker, 
                              color = speaker),
             size=3)+
  geom_text(data=heffalump,
            aes(label=speaker),
            hjust=1.15,
            vjust=-0.3, 
            size=7/.pt) +
  labs(
    title='Distribution of quotation position compared to length',
    subtitle = expression(paste('Selected characters from ',italic('Winnie-the-Pooh'),'by A.A. Milne')),
    caption = 'Data source: Project Dialogism Novel Corpus\nGraph ©2023 David Churchman, respects to C. Wilke'
       )


scatter

# Build the density plot for the top
xdens <- axis_canvas(scatter, axis = "x") +
  geom_density_line(
    data=pos_dens,
    aes(x = startingByte, y = density, 
        fill = speaker, color = speaker),
    stat = "identity", size = .5
  ) +
  geom_text(
    data = max_dens_pos,
    aes(
      label = label, 
      #hjust = hjust, 
      #vjust = vjust, 
      color = speaker,
      x = startingByte, 
      y = -0.0000025,
    ),
    family = 'Gill Sans', 
    size = 8/.pt 
  ) +
  scale_color_manual(
    values = colors,
    breaks = breaks,
    guide = "none"
  ) +
  scale_fill_manual(
    values = alpha(colors,alpha_d),
    breaks = breaks,
    guide = "none"
  ) +
  scale_y_continuous(limits = c(-0.000005, dens_limit_pos))
xdens

# Build the density plot for the Right
ydens <- axis_canvas(scatter, axis = "y") +
  geom_density_line(
    data=len_dens,
    aes(x = quoteLength, y = density, 
        fill = speaker, color = speaker),
    stat = "identity", size = .5
  ) +
 
  #  geom_text(
  #   data = max_dens_len,
  #   aes(
  #     label = label, 
  #     #hjust = hjust, 
  #     #vjust = vjust, 
  #     color = speaker,
  #     x = quoteLength, 
  #     y = density *1.05,
  #   ),
  #   angle=270,
  #   family = 'Gill Sans', 
  #   size = 6/.pt 
  # ) +
  scale_color_manual(
    values = colors,
    breaks = breaks,
    guide = "none"
  ) +
  scale_fill_manual(
    values = alpha(colors,alpha_d),
    breaks = breaks,
    guide = "none"
  ) +
  scale_y_continuous(limits = c(0, dens_limit_len))+
  coord_flip()
ydens


# combine density plots with scatter

p1 <- insert_xaxis_grob(
  scatter,
  xdens,
  position = "top"
)


p2 <- insert_yaxis_grob(p1, 
                        ydens, 
                        position = "right")
ggdraw(p2)
