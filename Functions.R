GGMetabolite <- function(data, meanC, meanH, treatment1, treatment2, xlabel){
  library(dplyr)
  library(ggplot2)
    Organic.order <- data %>% 
      arrange(-meanC) %>% 
      filter(metabplot ==treatment1)
    Mineral.order <- data %>% 
        arrange(-meanC) %>% 
        filter(metabplot == treatment2)
    ## ggplot for Organic horizon
    Organic.Metabolite.points <- ggplot(data = data %>% gather(treatment, abundance, c(meanC, meanH)) %>% filter(metabplot == "O"), aes(x=factor(Metabolite, levels=Organic.order$Metabolite), y=abundance, color = treatment, shape=p.category)) +
        geom_point() + 
        coord_flip() + 
        scale_shape_manual(values=c(17,16)) +  
        theme(axis.text.y =  element_text(size=2), axis.title.y=element_blank())
    ## ggplot for Mineral horizon
    Mineral.Metabolite.points <- ggplot(data = data %>% gather(treatment, abundance, c(meanC, meanH)) %>% filter(metabplot == "M"), aes(x=factor(Metabolite, levels=Mineral.order$Metabolite), y=abundance, color = treatment, shape=p.category)) +
      geom_point() + 
      coord_flip() + 
      scale_shape_manual(values=c(17,16)) + 
      theme(axis.text.y = element_text(size=2)) + xlab(xlabel)
    ## arrange both horizons next to each other.
    Metabolites.Figure <- ggarrange(Mineral.Metabolite.points, Organic.Metabolite.points, ncol=2, common.legend = TRUE, labels=c("A.", "B."))
    ## Display figure
    return(Metabolites.Figure)
}