#' DRG Boxplot
#'
#'This function produces boxplots of the IPPS Dataset for Payments from the 2011 Fiscal Year.
#'
#' @param p1 choose between 'Average.Covered.Charges', 'Average.Total.Payments', 'Average.Medicare.Payments' to produce a boxplot per each DRG Code. Case-sensitive and include quotations marks
#'
#' @return A boxplot plot of the input column data
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import stringr
#'
#' @examples
#' DRG_boxplot('Average.Covered.Charges')
#' DRG_boxplot('Average.Total.Payments')
#' DRG_boxplot('Average.Medicare.Payments')
#'
DRG_boxplot <- function(p1) {
  DRG_dataset <- DRG_dataset %>% mutate(DRG_short=substr(DRG.Definition,1,3))
  g <- ggplot(DRG_dataset,aes(x=DRG_short))+
    geom_boxplot(aes_string(y=p1))+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(str_to_title(gsub(".", " ", p1, fixed=TRUE)))+ #Add title
    xlab('DRG code Number')+ #Add x label
    ylab(gsub(".", " ", p1, fixed=TRUE))+ #Add y label
    scale_y_continuous(trans = 'log10', labels = scales::comma) #Log scale to show all values clearly
  return(g)
}
