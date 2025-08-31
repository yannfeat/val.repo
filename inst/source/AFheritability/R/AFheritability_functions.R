######### Background functions for plots
Counterfactual_prev_target_location <- function(p, b, k, h2){
  corrmatrix <- diag(2)
  corrmatrix[2,1] <- sqrt(h2)
  corrmatrix[1,2] <- sqrt(h2)
  
  beta <- qnorm(p)
  p_target <- as.numeric(pmvnorm(lower=c(-beta, b), upper=c(Inf, Inf), mean=rep(0, 2), corr=corrmatrix))
  lower1 <- -beta + k * sqrt(h2)
  lower2 <- b
  p_target_delta <- as.numeric(pmvnorm(lower=c(lower1, lower2), upper=c(Inf, Inf), mean=rep(0, 2), corr=corrmatrix))
  AF <- as.numeric((p_target - p_target_delta) / p)
  return(AF)
}

Counterfactual_prev_target_scale <- function(p, b, delta, h2){
  corrmatrix <- diag(2)
  corrmatrix[2,1] <- sqrt(h2)
  corrmatrix[1,2] <- sqrt(h2)
  
  beta <- qnorm(p, lower.tail = FALSE) 
  p_target <- as.numeric(pmvnorm(lower=c(beta, b), upper=c(Inf, Inf), mean=rep(0, 2), corr=corrmatrix))
  lower1 <- beta / sqrt(h2 / delta^2 + (1-h2))
  lower2 <- b
  p_target_delta <- as.numeric(pmvnorm(lower=c(lower1, lower2), upper=c(Inf, Inf), mean=rep(0, 2), corr=corrmatrix))
  AF <- as.numeric((p_target - p_target_delta) / p)
  return(AF)
}


######## To calculate counterfactual disease prevalence among the target group
Counterfactual_prev <- function(p, b, k, h2){
  corrmatrix <- diag(2)
  corrmatrix[2,1] <- sqrt(h2)
  corrmatrix[1,2] <- sqrt(h2)
  
  beta <- qnorm(p)
  p_target <- as.numeric(pmvnorm(lower=c(-beta, b), upper=c(Inf, Inf), mean=rep(0, 2), corr=corrmatrix))
  lower1 <- -beta + k * sqrt(h2)
  lower2 <- b
  p_target_delta <- as.numeric(pmvnorm(lower=c(lower1, lower2), upper=c(Inf, Inf), mean=rep(0, 2), corr=corrmatrix))
  p_new <- (p- p_target) + p_target_delta
  p_p <- p_target_delta/p_target
  out=list(p_target=p_target, p_target_delta=p_target_delta, p_new=p_new, p_p=p_p)
  return(out)
}

############ Translate quantiles into proportions

probabilities <- function(p) {
  
  percent <- function(x){
    probability <- x * 100
    if(probability >= 1) {probability <- round(probability, digits = 1)}
    if(probability < 1 & probability > 0.1) probability <- round(probability, digits = 2)
    if(probability <= 0.1 ) probability <- round(probability, digits = 3)
    probability <- paste(probability,"%")
    return(probability)
  }
  
  prob <- lapply(p, percent)
  prob <-  unlist(prob)
  
  return(prob)
}

######### xaxis = heritability

############ compare = prevalence
AF_heritability_prevalence <- function(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, legend_position, ...){
  AF <- matrix("list", nrow=length(Prevalence), ncol=length(Heritability))
  
  # Transform the proportion targeted into a quantile
  Target <- qnorm(Target, lower.tail = FALSE)
  
  for (j in 1:length(Heritability)){
    heritability <- Heritability[j]
    for (i in 1:length(Prevalence)){
      prevalence <- Prevalence[i]
      
      if (Intervention_type == "location")
        AF[i, j] <- Counterfactual_prev_target_location(p = prevalence, k = Intervention, b = Target, h2 = heritability)
      if (Intervention_type == "scale")
        AF[i, j] <- Counterfactual_prev_target_scale(p = prevalence, delta = Intervention, b = Target, h2 = heritability)
    }
  }
  
  AF <- t(AF)
  
  if(plot == TRUE){
    AF <- as.data.frame(AF)
    Prevalence_text <- probabilities(Prevalence)
    colnames(AF) <- Prevalence_text
    AF$Heritability <- Heritability
    plot_data <- melt(AF,id.var="Heritability")
    plot_data$value <- as.numeric(plot_data$value)
    colnames(plot_data) <- c("Heritability", "Prevalence", "AF")
    
    ## Plot
    plot_heritability <- ggplot(plot_data, aes(x=Heritability,y=AF,group=Prevalence,colour=Prevalence)) +
      geom_line(aes(linetype=Prevalence), size=1) +
      geom_point(aes(shape=Prevalence)) + theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            text = element_text(size=17))+
      labs(x=expression(paste("Heritability ", "(", h^2, ")")))
    
    print(plot_heritability)
    
    out <- list(AF = AF, plot_heritability = plot)
  }
  
  out <- list(AF = AF)
  return(out)
}

############ compare = target
AF_heritability_target <- function(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, legend_position, ylim, ...){
  AF <- matrix("list", nrow=length(Target), ncol=length(Heritability))
  
  Target_percent <- Target
  
  # Transform the proportion targeted into a quantile
  Target <- qnorm(Target, lower.tail = FALSE)
  
  for (j in 1:length(Heritability)){
    heritability <- Heritability[j]
    for (i in 1:length(Target)){
      target <- Target[i]
      
      if (Intervention_type == "location")
        AF[i, j] <- Counterfactual_prev_target_location(p = Prevalence, k = Intervention, b = target, h2 = heritability)
      if (Intervention_type == "scale")
        AF[i, j] <- Counterfactual_prev_target_scale(p = Prevalence, delta = Intervention, b = target, h2 = heritability)
    }
  }
  
  AF <- t(AF)
  
  if(plot == TRUE){
    AF <- as.data.frame(AF)
    Target_percent_text <- probabilities(Target_percent)
    colnames(AF) <- Target_percent_text
    AF$Heritability <- Heritability
    plot_data <- melt(AF,id.var="Heritability")
    plot_data$value <- as.numeric(plot_data$value)
    colnames(plot_data) <- c("Heritability", "Target_percent", "AF")
    
    ## Plot
    plot_heritability <- ggplot(plot_data, aes(x=Heritability, y=AF, group=Target_percent, 
                                               colour=Target_percent)) + 
      geom_line(aes(linetype=Target_percent), size=1) +
      geom_point(aes(shape=Target_percent), size =2) + theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            text = element_text(size=17)) +
      labs(x= expression(paste("Heritability ", "(", h^2, ")")), colour = "Targeted", lty= "Targeted", shape = "Targeted")
    
    print(plot_heritability)
    
    out <- list(AF = AF, plot_heritability = plot)
  }
  
  out <- list(AF = AF)
  return(out)
}

############ compare = intervention
AF_heritability_intervention <- function(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, legend_position, ylim, ...){
  AF <- matrix("list", nrow=length(Intervention), ncol=length(Heritability))
  
  # Transform the proportion targeted into a quantile
  Target <- qnorm(Target, lower.tail = FALSE)
  
  for (j in 1:length(Heritability)){
    heritability <- Heritability[j]
    for (i in 1:length(Intervention)){
      intervention <- Intervention[i]
      
      if (Intervention_type == "location")
        AF[i, j] <- Counterfactual_prev_target_location(p = Prevalence, k = intervention, b = Target, h2 = heritability)
      if (Intervention_type == "scale")
        AF[i, j] <- Counterfactual_prev_target_scale(p = Prevalence, delta = intervention, b = Target, h2 = heritability)
    }
  }
  
  AF <- t(AF)
  
  if(plot == TRUE){
    AF <- as.data.frame(AF)
    colnames(AF) <- Intervention
    AF$Heritability <- Heritability
    plot_data <- melt(AF,id.var="Heritability")
    plot_data$value <- as.numeric(plot_data$value)
    colnames(plot_data) <- c("Heritability", "Intervention", "AF")
    
    ## Plot
    plot_heritability <- ggplot(plot_data, aes(x=Heritability, y=AF, group=Intervention, 
                                               colour=Intervention)) +
      geom_line(aes(lty=Intervention), size=1)+
      geom_point(aes(shape=Intervention), size =2)+theme_bw()+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            text = element_text(size=17))+
      labs(x= expression(paste("Heritability ", "(", h^2, ")")))
    print(plot_heritability)
    
    out <- list(AF = AF, plot_heritability = plot)
  }
  
  out <- list(AF = AF)
  return(out)
}

######## xaxis = prevalence
############ compare = heritability
AF_prevalence_heritability <- function(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, legend_position, ylim, ...){
  AF <- matrix("list", nrow=length(Heritability), ncol=length(Prevalence))
  
  # Transform the proportion targeted into a quantile
  Target <- qnorm(Target, lower.tail = FALSE)
  
  for (j in 1:length(Prevalence)){
    prevalence <- Prevalence[j]
    for (i in 1:length(Heritability)){
      heritability <- Heritability[i]
      
      if (Intervention_type == "location")
        AF[i, j] <- Counterfactual_prev_target_location(p = prevalence, k = Intervention, b = Target, h2 = heritability)
      if (Intervention_type == "scale")
        AF[i, j] <- Counterfactual_prev_target_scale(p =prevalence, delta = Intervention, b = Target, h2 = heritability)
    }
  }
  
  AF <- t(AF)
  
  if(plot == TRUE){
    AF <- as.data.frame(AF)
    Heritability_text <- probabilities(Heritability)
    colnames(AF) <- Heritability_text
    AF$Prevalence <- Prevalence
    plot_data <- melt(AF,id.var="Prevalence")
    plot_data$value <- as.numeric(plot_data$value)
    colnames(plot_data) <- c("Prevalence", "Heritability", "AF")
    
    ## Plot
    plot_prevalence <- ggplot(plot_data, aes(x=Prevalence,y=AF,group=Heritability,colour=Heritability)) +
      geom_line(aes(lty=Heritability), size=1) +
      geom_point(aes(shape=Heritability), size =2)+ theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
            text = element_text(size=17))+
      labs(x= "Prevalence")
    print(plot_prevalence)
    
    out <- list(AF = AF, plot_prevalence = plot)
  }
  
  out <- list(AF = AF)
  return(out)
}
############ compare = target
AF_prevalence_target <- function(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, legend_position, ylim, ...){
  AF <- matrix("list", nrow=length(Target), ncol=length(Prevalence))
  
  Target_percent <- Target
  
  # Transform the proportion targeted into a quantile
  Target <- qnorm(Target, lower.tail = FALSE)
  
  for (j in 1:length(Prevalence)){
    prevalence <- Prevalence[j]
    for (i in 1:length(Target)){
      target <- Target[i]
      
      if (Intervention_type == "location")
        AF[i, j] <- Counterfactual_prev_target_location(p = prevalence, k = Intervention, b = target, h2 = Heritability)
      if (Intervention_type == "scale")
        AF[i, j] <- Counterfactual_prev_target_scale(p =prevalence, delta = Intervention, b = target, h2 = Heritability)
    }
  }
  
  AF <- t(AF)
  
  if(plot == TRUE){
    AF <- as.data.frame(AF)
    Target_percent_text <- probabilities(Target_percent)
    colnames(AF) <- Target_percent_text
    AF$Prevalence <- Prevalence
    plot_data <- melt(AF,id.var="Prevalence")
    plot_data$value <- as.numeric(plot_data$value)
    colnames(plot_data) <- c("Prevalence", "Target_percent", "AF")
    
    ## Plot
    plot_prevalence <- ggplot(plot_data, aes(x=Prevalence,y=AF,group=Target_percent,colour=Target_percent)) +
      geom_line(aes(lty=Target_percent), size=1) +
      geom_point(aes(shape=Target_percent), size =2) + theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
            text = element_text(size=17))+
      labs(x= "Prevalence", colour = "Targeted", lty="Targeted", shape="Targeted")
    print(plot_prevalence)
    
    out <- list(AF = AF, plot_prevalence = plot)
  }
  
  out <- list(AF = AF)
  return(out)
}

############ compare = intervention
AF_prevalence_intervention <- function(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, legend_position, ylim, ...){
  AF <- matrix("list", nrow=length(Intervention), ncol=length(Prevalence))
  
  # Transform the proportion targeted into a quantile
  Target <- qnorm(Target, lower.tail = FALSE)
  
  for (j in 1:length(Prevalence)){
    prevalence <- Prevalence[j]
    for (i in 1:length(Intervention)){
      intervention <- Intervention[i]
      
      if (Intervention_type == "location")
        AF[i, j] <- Counterfactual_prev_target_location(p = prevalence, k = intervention, b = Target, h2 = Heritability)
      if (Intervention_type == "scale")
        AF[i, j] <- Counterfactual_prev_target_scale(p =prevalence, delta = intervention, b = Target, h2 = Heritability)
    }
  }
  
  AF <- t(AF)
  
  if(plot == TRUE){
    AF <- as.data.frame(AF)
    colnames(AF) <- Intervention
    AF$Prevalence <- Prevalence
    plot_data <- melt(AF,id.var="Prevalence")
    plot_data$value <- as.numeric(plot_data$value)
    colnames(plot_data) <- c("Prevalence", "Intervention", "AF")
    
    ## Plot
    plot_prevalence <- ggplot(plot_data, aes(x=Prevalence,y=AF,group=Intervention,colour=Intervention)) +
      geom_line(aes(lty=Intervention), size=1) +
      geom_point(aes(shape=Intervention), size =2) + theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
            text = element_text(size=17))+
      labs(x= "Prevalence", colour = "Intervention", lty="Intervention", shape="Intervention")
    print(plot_prevalence)
    
    out <- list(AF = AF, plot_prevalence = plot)
  }
  
  out <- list(AF = AF)
  return(out)
}

####### xaxis=target
############ compare = heritability
AF_target_heritability <- function(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, legend_position, ylim, ...){
  AF <- matrix("list", nrow=length(Heritability), ncol=length(Target))
  
  Target_percent <- Target
  
  # Transform the proportion targeted into a quantile
  Target <- qnorm(Target, lower.tail = FALSE)
  
  for (j in 1:length(Target)){
    target <- Target[j]
    for (i in 1:length(Heritability)){
      heritability <- Heritability[i]
      
      if (Intervention_type == "location")
        AF[i, j] <- Counterfactual_prev_target_location(p = Prevalence, k = Intervention, b = target, h2 = heritability)
      if (Intervention_type == "scale")
        AF[i, j] <- Counterfactual_prev_target_scale(p =Prevalence, delta = Intervention, b = target, h2 = heritability)
    }
  }
  
  AF <- t(AF)
  
  if(plot == TRUE){
    AF <- as.data.frame(AF)
    Heritability_text <- probabilities(Heritability)
    colnames(AF) <- Heritability_text
    AF$Target_percent <- Target_percent
    plot_data <- melt(AF,id.var="Target_percent")
    plot_data$value <- as.numeric(plot_data$value)
    colnames(plot_data) <- c("Target_percent", "Heritability", "AF")
    
    ## Plot
    plot_target <- ggplot(plot_data, aes(x=Target_percent,y=AF,group=Heritability,colour=Heritability)) +
      geom_line(aes(lty=Heritability), size=1) + 
      geom_point(aes(shape=Heritability)) + theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
            text = element_text(size=17))+
      labs(x= "Proportion targeted")
    print(plot_target)
    
    out <- list(AF = AF, plot_target = plot)
  }
  
  out <- list(AF = AF)
  return(out)
}
############ compare = prevalence
AF_target_prevalence <- function(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, legend_position, ylim, ...){
  AF <- matrix("list", nrow=length(Prevalence), ncol=length(Target))
  
  Target_percent <- Target
  
  # Transform the proportion targeted into a quantile
  Target <- qnorm(Target, lower.tail = FALSE)
  
  for (j in 1:length(Target)){
    target <- Target[j]
    for (i in 1:length(Prevalence)){
      prevalence <- Prevalence[i]
      
      if (Intervention_type == "location")
        AF[i, j] <- Counterfactual_prev_target_location(p = prevalence, k = Intervention, b = target, h2 = Heritability)
      if (Intervention_type == "scale")
        AF[i, j] <- Counterfactual_prev_target_scale(p = prevalence, delta = Intervention, b = target, h2 = Heritability)
    }
  }
  
  AF <- t(AF)
  
  if(plot == TRUE){
    AF <- as.data.frame(AF)
    Prevalence_text <- probabilities(Prevalence)
    colnames(AF) <- Prevalence_text
    AF$Target_percent <- Target_percent
    plot_data <- melt(AF,id.var="Target_percent")
    plot_data$value <- as.numeric(plot_data$value)
    colnames(plot_data) <- c("Target_percent", "Prevalence", "AF")
    
    ## Plot
    plot_target <- ggplot(plot_data, aes(x=Target_percent,y=AF,group=Prevalence,colour=Prevalence)) +
      geom_line(aes(lty=Prevalence), size=1) +
      geom_point(aes(shape=Prevalence)) + theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
            text = element_text(size=17))+
      labs(x= "Proportion targeted")
    print(plot_target)
    
    out <- list(AF = AF, plot_target = plot)
  }
  
  out <- list(AF = AF)
  return(out)
}

############ compare = intervention
AF_target_intervention <- function(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, legend_position, ylim, ...){
  AF <- matrix("list", nrow=length(Intervention), ncol=length(Target))
  
  Target_percent <- Target
  
  # Transform the proportion targeted into a quantile
  Target <- qnorm(Target, lower.tail = FALSE)
  
  for (j in 1:length(Target)){
    target <- Target[j]
    for (i in 1:length(Intervention)){
      intervention <- Intervention[i]
      
      if (Intervention_type == "location")
        AF[i, j] <- Counterfactual_prev_target_location(p = Prevalence, k = intervention, b = target, h2 = Heritability)
      if (Intervention_type == "scale")
        AF[i, j] <- Counterfactual_prev_target_scale(p = Prevalence, delta = intervention, b = target, h2 = Heritability)
    }
  }
  
  AF <- t(AF)
  
  if(plot == TRUE){
    AF <- as.data.frame(AF)
    colnames(AF) <- Intervention
    AF$Target_percent <- Target_percent
    plot_data <- melt(AF,id.var="Target_percent")
    plot_data$value <- as.numeric(plot_data$value)
    colnames(plot_data) <- c("Target_percent", "Intervention", "AF")
    
    ## Plot
    plot_target <- ggplot(plot_data, aes(x=Target_percent,y=AF,group=Intervention,colour=Intervention)) +
      geom_line(aes(lty=Intervention), size=1) +
      geom_point(aes(shape=Intervention)) + theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
            text = element_text(size=17))+
      labs(x= "Proportion targeted")
    print(plot_target)
    
    out <- list(AF = AF, plot_target = plot)
  }
  
  out <- list(AF = AF)
  return(out)
}

######## xaxis = intervention
############ compare = heritability
AF_intervention_heritability <- function(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, legend_position, ylim, ...){
  AF <- matrix("list", nrow=length(Heritability), ncol=length(Intervention))
  
  # Transform the proportion targeted into a quantile
  Target <- qnorm(Target, lower.tail = FALSE)
  
  for (j in 1:length(Intervention)){
    intervention <- Intervention[j]
    for (i in 1:length(Heritability)){
      heritability <- Heritability[i]
      
      if (Intervention_type == "location")
        AF[i, j] <- Counterfactual_prev_target_location(p = Prevalence, k = intervention, b = Target, h2 = heritability)
      if (Intervention_type == "scale")
        AF[i, j] <- Counterfactual_prev_target_scale(p = Prevalence, delta = intervention, b = Target, h2 = heritability)
    }
  }
  
  AF <- t(AF)
  
  if(plot == TRUE){
    AF <- as.data.frame(AF)
    Heritability_text <- probabilities(Heritability)
    colnames(AF) <- Heritability_text
    AF$Intervention <- Intervention
    plot_data <- melt(AF,id.var="Intervention")
    plot_data$value <- as.numeric(plot_data$value)
    colnames(plot_data) <- c("Intervention", "Heritability", "AF")
    
    ## Plot
    plot_intervention <- ggplot(plot_data, aes(x=Intervention,y=AF,group=Heritability,colour=Heritability)) +
      geom_line(aes(lty=Heritability), size=1) +
      geom_point(aes(shape=Heritability)) + theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            text = element_text(size=17))+
      labs(x= "Intervention effect (k)")
    print(plot_intervention)
    
    out <- list(AF = AF, plot_intervention = plot)
  }
  
  out <- list(AF = AF)
  return(out)
}

############ compare = prevalence
AF_intervention_prevalence <- function(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, legend_position, ylim, ...){
  AF <- matrix("list", nrow=length(Prevalence), ncol=length(Intervention))
  
  # Transform the proportion targeted into a quantile
  Target <- qnorm(Target, lower.tail = FALSE)
  
  for (j in 1:length(Intervention)){
    intervention <- Intervention[j]
    for (i in 1:length(Prevalence)){
      prevalence <- Prevalence[i]
      
      if (Intervention_type == "location")
        AF[i, j] <- Counterfactual_prev_target_location(p = prevalence, k = intervention, b = Target, h2 = Heritability)
      if (Intervention_type == "scale")
        AF[i, j] <- Counterfactual_prev_target_scale(p = prevalence, delta = intervention, b = Target, h2 = Heritability)
    }
  }
  
  AF <- t(AF)
  
  if(plot == TRUE){
    AF <- as.data.frame(AF)
    Prevalence_text <- probabilities(Prevalence)
    colnames(AF) <- Prevalence_text
    AF$Intervention <- Intervention
    plot_data <- melt(AF,id.var="Intervention")
    plot_data$value <- as.numeric(plot_data$value)
    colnames(plot_data) <- c("Intervention", "Prevalence", "AF")
    
    ## Plot
    plot_intervention <- ggplot(plot_data, aes(x=Intervention,y=AF,group=Prevalence,colour=Prevalence)) +
      geom_line(aes(lty=Prevalence), size=1) +
      geom_point(aes(shape=Prevalence))+ theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            text = element_text(size=17))+
      labs(x= "Intervention effect (k)")
    print(plot_intervention)
    
    out <- list(AF = AF, plot_intervention = plot)
  }
  
  out <- list(AF = AF)
  return(out)
}

############ compare = target
AF_intervention_target <- function(Prevalence, Heritability, Target, Intervention, Intervention_type, plot = TRUE, legend = TRUE, Disease, legend_position, ylim, ...){
  AF <- matrix("list", nrow=length(Target), ncol=length(Intervention))
  
  Target_percent <- Target
  
  # Transform the proportion targeted into a quantile
  Target <- qnorm(Target, lower.tail = FALSE)
  
  for (j in 1:length(Intervention)){
    intervention <- Intervention[j]
    for (i in 1:length(Target)){
      target <- Target[i]
      if (Intervention_type == "location")
        AF[i, j] <- Counterfactual_prev_target_location(p = Prevalence, k = intervention, b = target, h2 = Heritability)
      if (Intervention_type == "scale")
        AF[i, j] <- Counterfactual_prev_target_scale(p = Prevalence, delta = intervention, b = target, h2 = Heritability)
    }
  }
  
  AF <- t(AF)
  
  if(plot == TRUE){
    AF <- as.data.frame(AF)
    Target_percent_text <- probabilities(Target_percent)
    colnames(AF) <- Target_percent_text
    AF$Intervention <- Intervention
    plot_data <- melt(AF,id.var="Intervention")
    plot_data$value <- as.numeric(plot_data$value)
    colnames(plot_data) <- c("Intervention", "Target_percent", "AF")
    
    ## Plot
    plot_intervention <- ggplot(plot_data, aes(x=Intervention, y=AF, group=Target_percent, 
                                               colour=Target_percent)) + 
      geom_line(aes(linetype=Target_percent), size=1) +
      geom_point(aes(shape=Target_percent), size =2) + theme_bw()+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            text = element_text(size=17))+
      labs(x= "Intervention effect (k)", colour = "Targeted", lty="Targeted", shape="Targeted")
    print(plot_intervention)
    
    out <- list(AF = AF, plot_intervention = plot)
  }
  
  out <- list(AF = AF)
  return(out)
}