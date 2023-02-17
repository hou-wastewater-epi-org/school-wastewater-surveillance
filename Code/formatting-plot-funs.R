#######################################################################
#
# HHD+Rice CDC Center of Excellence for Wastewater Epidemiology
#?https://hou-wastewater-epi.org
# Contact email:?info@hou-wastewater-epi.org
#
# Paper: "Wastewater surveillance of SARS-CoV-2 and influenza in preK-12?
#               schools shows school, community, and citywide infections."
#
# PI of Analytics Group: Dr. Katherine B. Ensor, Department of Statistics, Rice University
# Principal programmer for paper: Thomas Sun, Department of Statistics, Rice University
# Principal programmer for HHD: Rebecca Schneider, Houston Health Department
#
# Code is shared under a GPL-3 License. See LICENSE file in Code folder.
# Data is shared under a CC by-NC-SA license. see LICENSE file in Data folder.
#
#######################################################################

# formatting plots

# plot formatting ---- 


# Set theme universally : format as classic, colours = Dark2
theme_set(theme_classic()) # theme
scale_colour_discrete <- function(...) { # palette for colours (outlines, ..)
  scale_colour_brewer(..., palette="Dark2")
}

scale_fill_discrete <- function(...) { # palette for fills
  scale_fill_brewer(..., palette="Dark2")
}


# plot formatting function : format as classic, colours = Set1
format_classic <- function(plt)
{ # formats plot as classic, with colour palette Set1, centred title, angled x axis labels
  plt <- plt +
    theme_classic() + scale_color_brewer(palette="Set1")
}


# plot formatting function : format as logscale
format_logscale_y <- function(plt)
{ # extra comments
  plt <- plt +
    scale_y_log10(  # logscale for y axis with tick marks
      labels = fancy_scientific
      #labels = scales::trans_format("log10", scales::math_format(10^.x) )
    )
}

# plot formatting function : format as logscale x
format_logscale_x <- function(plt)
{ # extra comments
  plt <- plt +
    scale_x_log10(  # logscale for y axis with tick marks
      labels = fancy_scientific
      #labels = scales::trans_format("log10", scales::math_format(10^.x) )
    )
}


# formatting labels in logscale cleanly : a x 10^b
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # remove + after exponent, if exists. E.g.: (3x10^+2 -> 3x10^2) 
  l <- gsub("e\\+","e",l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # convert 1x10^ or 1.000x10^ -> 10^ 
  l <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", l)
  # return this as an expression
  parse(text=l)
}

# use as ggplot(df,aes(x,y)) + geom_point() + scale_y_log10(labels = fancy_scientific)

