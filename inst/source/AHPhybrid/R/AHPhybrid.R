AHPhybrid <- function(title, Alternatives, Qualitative_criteria, Quantitative_criteria,
                      Quantitative_crit_min_max, n_alt, n_crit, n_crit_Qual, n_crit_Quant,
                      Criteria_Comparison, Alternatives_comparison_qualit_crit, Alternatives_quantitative_crit) {


  print(title)


  if((n_alt <= 2) && (n_crit <= 2) ){

    print("For implementation is necessary a minimum of 02 alternatives and 02 criteria")

  }else{



    ###Criteria Evaluation

    sum_col <- c()

    for (j in 1:n_crit) {

      sum_col_value = 0

      for(i in 1:n_crit){

        sum_col_value = sum_col_value + Criteria_Comparison[i,j]

      }

      sum_col <- append(sum_col, sum_col_value)

    }

    #Normalizing

    value_normalized <- c()

    for (j in 1:n_crit) {

      for(i in 1:n_crit){

        value = Criteria_Comparison[j,i]/sum_col[i]

        value_normalized <- append(value_normalized, value)
      }

    }

    criteria_normalized <- matrix(value_normalized, ncol = n_crit, nrow = n_crit, byrow = TRUE)


    #Priority Obtaining

    priority_crit <- c()

    for (j in 1:n_crit) {

      sum_row_value = 0

      for(i in 1:n_crit){

        sum_row_value = sum_row_value + criteria_normalized[j,i]

      }

      priority_value <- round((sum_row_value/n_crit),3)

      priority_crit <- append(priority_crit, priority_value)

    }

    criteria <- c()

    criteria <- append(Qualitative_criteria, Quantitative_criteria)

    priority_crit_table <- data.frame(criteria, priority_crit)


    #Consistency Evaluation

    weight_cons_value <- c()

    for (j in 1:n_crit) {

      for(i in 1:n_crit){

        value = Criteria_Comparison[j,i]*priority_crit[i]

        weight_cons_value <- append(weight_cons_value, value)
      }

    }

    weight_cons_value <- matrix(weight_cons_value, ncol = n_crit, nrow = n_crit, byrow = TRUE)


    sum_weight <- c()

    for (j in 1:n_crit) {

      sum_row_value = 0

      for(i in 1:n_crit){

        sum_row_value = sum_row_value + weight_cons_value[j,i]

      }

      sum_weight <- append(sum_weight, sum_row_value)

    }

    sum_lambda = 0

    for (j in 1:n_crit) {

      sum_lambda = sum_lambda + (sum_weight[j]/priority_crit[j])

    }

    max_lambda = sum_lambda/n_crit


    max_lambda

    cons_index = (max_lambda - n_crit)/(n_crit - 1)

    radio_index <- c(0,	0, 0.58,	0.9,	1.12,	1.24,	1.32,	1.41,	1.45,	1.49,	1.51,	1.48,	1.56,	1.57,	1.59)

    ri = radio_index[n_crit]

    consistency_criteria <- round((cons_index / ri), 3)

    consistency_criteria


    print("")
    print("")
    print("===== Criteria Priorities:")
    print("")
    print(priority_crit_table)
    print("")

    print(paste("The consistency ratio is:", consistency_criteria))
    print("")

    if (consistency_criteria <= 0.1) {

      print("The assignments are consistent.")

    } else {
      print("The assignments are not consistent.")
    }
    print("")
    print("")
    print("")
    print("")




    n_crit_Qual


    ### Alternatives Evaluation

    Alternatives_priorities <- c()



    ## in Qualitative Criteria

    if (length(Qualitative_criteria)>=1){



      for (k in 1:n_crit_Qual) {


        sum_col_alt <- c()

        for (j in 1:n_alt) {

          sum_col_value = 0

          for(i in 1:n_alt){

            sum_col_value = sum_col_value + Alternatives_comparison_qualit_crit[[k]][i,j]

          }

          sum_col_alt <- append(sum_col_alt, sum_col_value)

        }


        #Normalizing

        value_normalized_alt <- c()

        for (j in 1:n_alt) {

          for(i in 1:n_alt){

            value = Alternatives_comparison_qualit_crit[[k]][j,i]/sum_col_alt[i]

            value_normalized_alt <- append(value_normalized_alt, value)
          }

        }

        alt_normalized <- matrix(value_normalized_alt, ncol = n_alt, nrow = n_alt, byrow = TRUE)


        #Priority Obtaining

        priority_alt_crit <- c()

        for (j in 1:n_alt) {

          sum_row_value = 0

          for(i in 1:n_alt){

            sum_row_value = sum_row_value + alt_normalized[j,i]

          }

          priority_value <- sum_row_value/n_alt

          priority_alt_crit <- append(priority_alt_crit, priority_value)

        }

        priority_alt_table <- data.frame(Alternatives, priority_alt_crit)



        #Consistency Evaluation

        if (n_alt <= 2) {

          consistency_alt_crit <- 0

        }else{


          weight_cons_value_alt <- c()

          for (j in 1:n_alt) {

            for(i in 1:n_alt){

              value = Alternatives_comparison_qualit_crit[[k]][j,i]*priority_alt_crit[i]

              weight_cons_value_alt <- append(weight_cons_value_alt, value)
            }

          }

          weight_cons_value_alt <- matrix(weight_cons_value_alt, ncol = n_alt, nrow = n_alt, byrow = TRUE)


          sum_weight <- c()

          for (j in 1:n_alt) {

            sum_row_value = 0

            for(i in 1:n_alt){

              sum_row_value = sum_row_value + weight_cons_value_alt[j,i]

            }

            sum_weight <- append(sum_weight, sum_row_value)

          }

          sum_lambda = 0

          for (j in 1:n_alt) {

            sum_lambda = sum_lambda + (sum_weight[j]/priority_alt_crit[j])

          }

          max_lambda = sum_lambda/n_alt


          cons_index_alt_crit = (max_lambda - n_alt)/(n_alt - 1)

          radio_index <- c(0,	0, 0.58,	0.9,	1.12,	1.24,	1.32,	1.41,	1.45,	1.49,	1.51,	1.48,	1.56,	1.57,	1.59)

          ri = radio_index[n_alt]

          consistency_alt_crit <- (cons_index_alt_crit / ri)



        }


        #Printing evaluation in qualitative criteria

        print("")
        print("")
        print(paste("=== Alternatives Priorities in Criterion", Qualitative_criteria[k],":"))
        print("")
        print(priority_alt_table)
        print("")

        print(paste("The consistency ratio is:", round(consistency_alt_crit,3)))

        if (consistency_alt_crit <= 0.1) {

          print("The assignments are consistent.")

        } else {
          print("The assignments are not consistent.")
        }
        print("")


        #Saving all priorities

        Alternatives_priorities <- append(Alternatives_priorities, priority_alt_crit )

      }

    }


    # in Quantitative Criteria

    if (length(Quantitative_criteria)>=1){


      for (k in 1:n_crit_Quant) {

        sum_col = 0

        for (i in 1:n_alt) {

          if(Quantitative_crit_min_max[k] == "min"){

            sum_col = sum_col + (1/Alternatives_quantitative_crit[i,k])


          }else{

            sum_col = sum_col + Alternatives_quantitative_crit[i,k]

          }
        }

        priorities_simple <- c()

        for (i in 1:n_alt) {

          if(Quantitative_crit_min_max[k] == "min"){

            alt_crit_quant_norm = (1/Alternatives_quantitative_crit[i,k])/sum_col

          }else{


            alt_crit_quant_norm = Alternatives_quantitative_crit[i,k]/sum_col

          }

          priorities_simple <- append(priorities_simple, alt_crit_quant_norm )

          Alternatives_priorities <- append(Alternatives_priorities, alt_crit_quant_norm )

        }

        priority_alt_table <- data.frame(Alternatives, priorities_simple)

        print("")
        print("")
        print(paste("=== Alternatives Priorities in Criterion", Quantitative_criteria[k],":"))
        print("")
        print(priority_alt_table)
        print("")


      }

    }


    Alternatives_priorities <- matrix(Alternatives_priorities, ncol = n_crit, nrow = n_alt)
    rownames(Alternatives_priorities) <- Alternatives
    colnames(Alternatives_priorities) <- criteria
    print("")
    print("")
    print("")
    print("")
    print("=====Alternatives priorities for each criterion:")
    print(Alternatives_priorities)

    ###Aggregation Process

    values_aggreg <- c()

    for (j in 1:n_alt) {

      for (i in 1:n_crit) {

        value = round((priority_crit[i] * Alternatives_priorities[j,i]),3)

        values_aggreg <- append(values_aggreg, value)

      }

    }

    print("")
    print("")
    print("")
    print("")

    print("===== Global Index :")

    values_aggreg <- matrix(values_aggreg, ncol = n_crit, nrow = n_alt, byrow = TRUE)
    rownames(values_aggreg) <- Alternatives
    colnames(values_aggreg) <- criteria


    values_aggreg

    global_preference <- c()

    for (j in 1:n_alt) {

      sum_preference = 0

      for (i in 1:n_crit) {

        sum_preference = sum_preference + values_aggreg[j,i]

      }

      global_preference <- append(global_preference, sum_preference)
    }

    print("Final Results")

    Final_Result <- data.frame(Alternatives, global_preference)

    ordering <- sort(global_preference, decreasing = TRUE)

    for(i in 1:n_alt){
      print(paste(Alternatives[match(ordering[i],global_preference)],'=',ordering[i]))
    }



  }



}
