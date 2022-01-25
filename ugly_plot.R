library(tidyverse)
library(ggplot2)
library(hexbin)

sweaters <- read_csv(here::here("data/use_this_data", 
                                "holiday_sweaters-2020-12-15-clean.csv")) %>%
filter(hs_tf == "Yes") %>% 
  # potentially useful function: separate colors column into rows
  separate_rows(colors, sep = c(", ")) %>% 
  group_by(sweater) %>% 
  mutate(sum = length(colors)) %>% 
  rownames_to_column("values")

sweaters$colors <- NULL
sweaters$values <- NULL
sweaters$image_desc <- NULL
sweaters <- unique(sweaters)

#for each sweater, find the number of Yes's in categories by sweater

total_yes = function(x, output){
  columns = c(2, 3, 4, 5, 6, 7)
  total <- 0
  for (col in columns) {
    ifelse(x[col] == "Yes", total <- total + 1, NA)
  }
  
  return(total)
}

total_yes_col <- apply(sweaters, 1, total_yes)

#append the number of yes's to the sweaters data

sweaters <- cbind(sweaters, total_yes = total_yes_col)




#plot with sweater number, color, and number of Yes's

ggplot(sweaters, aes(x=sweater, y=sum, col=total_yes)) + geom_point() + 
  ggtitle("Lovely Plot of Sweaters by Number of features and Number of Colors") +
  ylab("Total of features (by Yes in data)") + xlab("Sweater Entry") + 
  theme_dark()




ggsave(here::here("ugly-plot.pdf"), plot, filepath, width, height)


                     
                     

                     
