#let's source some packages, listed in one file
source("code/packages_and_functions.R")


# Load and process the data -----------------------------------------------


Ca <- read_csv("data/WTvsNOS11_cPRC_INNOS.csv")

Ca %>% 
  ggplot(aes(x = frame, y = intesnsity, color = phenotype)) +
  geom_smooth() +
  theme_classic() +
  facet_wrap(vars(cell))

#save the plot
ggsave("pictures/Ca_WT_NOS_Kei.png", 
       width = 1700, 
       height = 1400, limitsize = TRUE, 
       units = c("px"))


#read some pre-processed data text/csv file from the /data directory
syn <- read_csv2("data/head_celltypes_syn_matrix.csv")

syn

#convert the matrix into a tibble in tidy format
syn_tb <- syn %>%
  rename ("presyn_cell" = ...1) %>%
  pivot_longer(-presyn_cell, names_to = "postsyn_cell", values_to = "synapses") %>%
  mutate(synapse_fraction = synapses / sum(synapses, na.rm = TRUE))

syn_tb

# visualise the data and save plot ----------------------------------------

#plot with ggplot
ggplot(syn_tb) +
  geom_point(aes(x = postsyn_cell, y = presyn_cell, size = sqrt(synapses), color = synapse_fraction), 
             stroke = 0,)+ 
  theme(
    axis.text.x = element_text (angle = 90,hjust = 1, vjust = 0.5, size=3), 
    axis.text.y = element_text (angle = 0,hjust = 1, vjust = 0.5, size=3),
    axis.title.x = element_text (size=14),
    axis.title.y = element_text (size=14)
  ) + 
  labs(x="postsynaptic cell types",y="presynaptic cell types",title=" ") +
  scale_size_area(max_size=1) +
  guides(color = 'legend') +
  scale_colour_gradient2(
    low = "#0072B2",
    mid = "#D55E00",
    high ="#D55E00",
    midpoint = 0.5,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  theme(panel.background = element_rect(fill = "grey98", color = "white"))


# Save the plot as pdf and png
ggsave("pictures/head_celltypes_syn_matrix.pdf", 
       width = 2000, 
       height = 1600, limitsize = TRUE, 
       units = c("px"))
ggsave("pictures/head_celltypes_syn_matrix.png", 
       width = 1700, 
       height = 1400, limitsize = TRUE, 
       units = c("px"))


# save the table as supplementary file ------------------------------------
#write table
readr::write_csv(syn_tb, file="supplements/Supplementary_table1.csv", na="", quote="none")



# assemble figure ---------------------------------------------------------

#read the pictures from the /pictures folder
img1 <- readPNG("pictures/Platynereis_SEM_inverted_nolabel.png")
img2 <- readPNG("pictures/head_celltypes_syn_matrix.png")
img3 <- readPNG("pictures/iris_test.png")
img4 <- readPNG("pictures/MC3cover-200um.png")
img5 <- readPNG("pictures/Ca_WT_NOS_Kei.png")

#convert to image panel and add text labels with cowplot::draw_image 
panelA <- cowplot::ggdraw() + cowplot::draw_image(img1, scale = 1) + 
  draw_label("Platynereis larva", x = 0.35, y = 0.99, fontfamily = "sans", fontface = "plain",
             color = "black", size = 11, angle = 0, lineheight = 0.9, alpha = 1) +
  draw_label(expression(paste("50 ", mu, "m")), x = 0.27, y = 0.05, fontfamily = "sans", fontface = "plain",
             color = "black", size = 10, angle = 0, lineheight = 0.9, alpha = 1) + 
  draw_label("head", x = 0.5, y = 0.85, fontfamily = "sans", fontface = "plain",
             color = "black", size = 9, angle = 0, lineheight = 0.9, alpha = 1) + 
  draw_label("sg0", x = 0.52, y = 0.67, fontfamily = "sans", fontface = "plain",
             color = "black", size = 9, angle = 0, lineheight = 0.9, alpha = 1) + 
  draw_label("sg1", x = 0.51, y = 0.55, fontfamily = "sans", fontface = "plain",
             color = "black", size = 9, angle = 0, lineheight = 0.9, alpha = 1) + 
  draw_label("sg2", x = 0.52, y = 0.4, fontfamily = "sans", fontface = "plain",
             color = "black", size = 9, angle = 0, lineheight = 0.9, alpha = 1) + 
  draw_label("sg3", x = 0.54, y = 0.2, fontfamily = "sans", fontface = "plain",
             color = "black", size = 9, angle = 0, lineheight = 0.9, alpha = 1) + 
  draw_label("pygidium", x = 0.55, y = 0.03, fontfamily = "sans", fontface = "plain",
             color = "black", size = 9, angle = 0, lineheight = 0.9, alpha = 1)
panelB <- ggdraw() + draw_image(img2, scale = 1) + 
  draw_label("Synaptic connectivity", x = 0.4, y = 0.99, fontfamily = "sans", fontface = "plain",
             color = "black", size = 11, angle = 0, lineheight = 0.9, alpha = 1)

panelC <- ggdraw() + draw_image(img3, scale = 1)

panelD <- ggdraw() + draw_image(img4, scale = 1) + 
  draw_line(x = c(0.1, 0.3), y = c(0.07, 0.07), color = "black", size = 0.5) +
  draw_label(expression(paste("40 ", mu, "m")), x = 0.2, y = 0.1, fontfamily = "sans", fontface = "plain",
             color = "black", size = 10, angle = 0, lineheight = 0.9, alpha = 1) +
  draw_label("MC3cover neurons", x = 0.45, y = 0.99, fontfamily = "sans", fontface = "plain",
             size = 11, lineheight = 0.9)

panelE <- ggdraw() + draw_image(img5)
  
#clear the images from memory
#rm(img1, img2, img3)


# assemble final  figure with patchwork -----------------------------------

#we use the very efficient and elegant patchwork package to assemble the 
#multi-panel figure

#first, we define the layout with textual representation
# the # symbol will introduce an empty field - can be used to add spacers,
#their width will be defined later with the widths/heights options

layout <- "
ABCDE
"

#panels of different sizes
layout2 <- "
AABCD
AABEE
"
#you get the picture - you can define any layout and ratios with this representation

#we now define which panel go into the figure and in which order, the layout
#will be based on our textual definition above
#we can also define the relative sizes of rows and columns and the tags to use
#it is not necessary to enter all the tags e.g., c("A", "B", "C", "D"), patchwork
#takes care of that if we define the tag_levels only
Figure1 <- panelA + panelB + panelC + panelD + panelE +
  patchwork::plot_layout(design = layout, heights = c(1, 1)) + #we can change the heights of the rows in our layout (widths also can be defined)
  patchwork::plot_annotation(tag_levels = "A") &  #we can change this to 'a' for small caps or 'i' or '1'
  ggplot2::theme(plot.tag = element_text(size = 12, face='plain')) #or 'bold', 'italic'


# save figure -------------------------------------------------------------

#when you save the figure as pdf and png it is important to set the width and
#height of the figure properly, so that your panels fit nicely - you may have to try a few times

ggsave("figures/Figure1.pdf", limitsize = FALSE, 
       units = c("px"), Figure1, width = 4000, height = 800)
ggsave("figures/Figure1.png", limitsize = FALSE, 
       units = c("px"), Figure1, width = 4000, height = 800, bg = "white")



