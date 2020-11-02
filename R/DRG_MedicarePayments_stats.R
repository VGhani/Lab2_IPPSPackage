#' Statistics on Total Medicare Payments
#'
#'This function produces statistics about the Total Medicare Payments from IPPS Payments from the Fiscal Year 2011.
#'
#' @param p1 choose between 'Mean', 'Median', 'Standard Deviation' to return the respective statistic for each DRG code. Include quotation marks and case-sensetive.
#'
#' @return A list of called statistics for each DRG code
#' @export
#'
#'
#' @import dplyr
#'
#'
#' @examples
#' DRG_MedicarePayments_stats('Mean')
#' DRG_MedicarePayments_stats('Median')
#' DRG_MedicarePayments_stats('Standard Deviation')
#'
DRG_MedicarePayments_stats <- function(p1){
  DRGstats_table <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(DRGstats_table) <- c('DRG Code', 'Mean', 'Standard Deviation', 'Median')
  DRG_codes <- DRG_dataset %>% group_by(DRG.Definition) %>% summarise()
  DRG_codes<- data.frame(lapply(DRG_codes, as.character), stringsAsFactors=FALSE)

  for (i in 1:length(DRG_codes[[1]])) {
    DRG_temp <- DRG_dataset%>%filter(DRG.Definition==DRG_codes[[1]][i])
    DRGstats_table[i,1] <- DRG_codes[[1]][i]
    DRGstats_table[i,2] <- mean(DRG_temp$Average.Medicare.Payments, na.rm = TRUE)
    DRGstats_table[i,3] <- sd(DRG_temp$Average.Medicare.Payments, na.rm = TRUE)
    DRGstats_table[i,4] <- median(DRG_temp$Average.Medicare.Payments, na.rm = TRUE)
  }
  return(DRGstats_table[c('DRG Code',p1)])
}
