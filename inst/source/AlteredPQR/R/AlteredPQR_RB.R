
AlteredPQR_RB <- function (modif_z_score_threshold = 3.5, fraction_of_samples_threshold = 0.10, modif = 1, filter_variable_in_ref_set = "NO", write_table = "NO", print_recomm = "NO", quant_data_all_local = quant_data_all, cols_with_reference_data_local = cols_with_reference_data) {

    #globalVariables(c("quant_data_all", "cols_with_reference_data"))
    all_columns = 1:ncol(quant_data_all_local)
    cols_with_other_data = all_columns[!(all_columns%in%cols_with_reference_data_local)]

    if (!(is.na(cols_with_reference_data_local[1])))
    {
        quant_data_ref_set = quant_data_all_local[, cols_with_reference_data_local]
        quant_data_disease = quant_data_all_local[, cols_with_other_data]
    }
    #print ("Running")

    ### Quantitative values for proteins in the annotated and inferred protein complexes
    measured_proteins = row.names (quant_data_all_local)
    protA_ok <- int_pairs$ProtA %in% measured_proteins
    protB_ok <- int_pairs$ProtB %in% measured_proteins
    ok_elements = protA_ok & protB_ok
    int_pairs <- int_pairs[ok_elements,c(1,2)]

    protsA_ids = as.character (int_pairs$ProtA)
    protsB_ids = as.character (int_pairs$ProtB)
    values_protA = quant_data_ref_set[protsA_ids,]
    values_protB = quant_data_ref_set[protsB_ids,]


    ### We check which protein in the pair has higher values in the reference samples and this one becomes Prot1.
    av_A = apply (values_protA, 1, function(x)  mean(x, na.rm = TRUE))
    av_B = apply (values_protB, 1, function(x)  mean(x, na.rm = TRUE))

    diffAB = values_protA - values_protB
    diffBA = values_protB - values_protA

    pairsAB = paste(int_pairs[,1], int_pairs[,2], sep = "-")
    rownames (diffAB) = pairsAB
    pairsBA = paste(int_pairs[,2], int_pairs[,1], sep = "-")
    rownames (diffBA) = pairsBA

    diff_prot_pairs = diffAB
    positions_to_change = av_A < av_B
    rownames (diff_prot_pairs)[positions_to_change] = rownames (diffBA)[positions_to_change]
    diff_prot_pairs[positions_to_change,] = diffBA[positions_to_change,]

    ### For later on: check there are more than 3 samples with measurments for each pair
    measured_samples = apply (diff_prot_pairs, 1, function (x) length (x[!is.na(x)]))
    measured_samples = measured_samples [measured_samples > 3]

    ### Calculate Median and MAD for the reference set
    results_table = matrix (nrow=nrow(diff_prot_pairs),ncol=6)
    row.names (results_table) = rownames (diff_prot_pairs)
    results_table [,1] = apply (diff_prot_pairs, 1, function (x) median (x, na.rm = TRUE))
    results_table [,2] = apply (diff_prot_pairs, 1, function (x) mad (x, constant = 1, na.rm = TRUE))

    ### And also check variability in the reference set
    results_table [,3] = results_table [,1] - (2*1.4826*results_table [,2])
    results_table [,4] = results_table [,1] + (2*1.4826*results_table [,2])
    n_reference_samples = length (cols_with_reference_data_local)
    temp = diff_prot_pairs < results_table[,3]
    results_table [,5] = apply (temp, 1, function (x) sum(x, na.rm = T))
    temp = diff_prot_pairs > results_table[,4]
    results_table [,6] = apply (temp, 1, function (x) sum(x, na.rm = T))
    allowed_n_variable = n_reference_samples*0.2 ### It does not need to be 0.2 (i.e. 20%)
    var_pairs1 = row.names (results_table [ results_table [,5] >= allowed_n_variable, ])
    var_pairs2 = row.names (results_table [ results_table [,6] >= allowed_n_variable, ])
    var_pairs = unique(var_pairs1, var_pairs2)


    ### For each pair, calculate the Modified z-score in each disease sample (i.e. its distance to the corresponding ref samples)
    protsA_ids = as.character (int_pairs$ProtA)
    protsB_ids = as.character (int_pairs$ProtB)
    values_protA = quant_data_disease[protsA_ids,]
    values_protB = quant_data_disease[protsB_ids,]

    diffAB_anal = values_protA - values_protB
    diffBA_anal = values_protB - values_protA

    modif_z_score = diffAB_anal
    modif_z_score[positions_to_change, ] = diffBA_anal[positions_to_change, ]
    rownames (modif_z_score) = rownames (diff_prot_pairs)
    modif_z_score = (0.6745*(modif_z_score - results_table [,1]))/results_table [,2]

    ### Based on the defined thresholds, identify all pairs that have a stronger signal
    score_threshold = modif_z_score_threshold
    thresh_neg = -score_threshold
    n_samples_threshold = fraction_of_samples_threshold * (length (cols_with_other_data))
    signif_z_score_high = modif_z_score>score_threshold
    signif_z_score_low = modif_z_score<thresh_neg
    per.row.high = apply (signif_z_score_high, 1, function (x) sum(x, na.rm = TRUE))
    per.row.low = apply (signif_z_score_low, 1, function (x) sum(x, na.rm = TRUE))
    signif.row.high = per.row.high [per.row.high >= n_samples_threshold]
    signif.row.low = per.row.low [per.row.low >= n_samples_threshold]
    n_h = names (signif.row.high)
    n_l = names (signif.row.low)
    signif.pairs = unique(c(n_h, n_l))


    ### Indentify proteins that were driving the signal, i.e. that showed at least a small variability in expression in the significant disease samples
    proteins_values = matrix (nrow=nrow(quant_data_ref_set),ncol=4)
    row.names (proteins_values) = rownames (quant_data_ref_set)
    proteins_values [,1] = apply (quant_data_ref_set, 1, function (x) median (x, na.rm = TRUE))
    proteins_values [,2] = apply (quant_data_ref_set, 1, function (x) mad (x, constant = 1, na.rm = TRUE))

    ### When the overall varibility in protein measurements is high 'modif' can be higher than 1
    # default: modif = 1
    proteins_values [,3] = proteins_values [,1] - (modif*1.4826*proteins_values [,2]) # LOWER_THRESH_PROT, it can also be:  2*1.4826*proteins_values [,2]
    proteins_values [,4] = proteins_values [,1] + (modif*1.4826*proteins_values [,2]) # UPPER_THRESH_PROT, it can also be:  2*1.4826*proteins_values [,2]

    ### Check individually all significant samples and protein pairs.
    ### For a protein to be considered as driving the signal, its quantity had to slightly change in at least half of the significant samples.
    #print ("...")
    signif_indiv_prots = list()
    for (i in signif.pairs)
    {
        values_z_scores = as.numeric(modif_z_score[i,])
        signif_prots <- unlist (strsplit(i, "-"))
        protA = signif_prots[1]
        protB = signif_prots[2]

        if (i %in% n_h)
        {
            signif_samples_high  = which(values_z_scores > score_threshold)
            n_signif_samples = length (signif_samples_high)
            protA_values = quant_data_disease[protA, signif_samples_high]
            protB_values = quant_data_disease[protB, signif_samples_high]

            A_below_limit = protA_values [protA_values  < proteins_values[protA,3]]
            A_above_limit = protA_values [protA_values  > proteins_values[protA,4]]
            if ((length(A_below_limit) >= (n_signif_samples*0.5)) || (length(A_above_limit) >= (n_signif_samples*0.5)))
            {
                signif_indiv_prots[[protA]][i] = sum (values_z_scores[signif_samples_high], na.rm = T)
            }

            B_below_limit = protB_values [protB_values  < proteins_values[protB,3]]
            B_above_limit = protB_values [protB_values  > proteins_values[protB,4]]
            if ((length(B_below_limit) >= (n_signif_samples*0.5)) || (length (B_above_limit) >= (n_signif_samples*0.5)))
            {
                signif_indiv_prots[[protB]][i] = sum (values_z_scores[signif_samples_high], na.rm = T)
            }
        }

        if (i %in% n_l)
        {
            signif_samples_low  = which(values_z_scores < thresh_neg)
            n_signif_samples = length (signif_samples_low)
            protA_values = quant_data_disease[protA, signif_samples_low]
            protB_values = quant_data_disease[protB, signif_samples_low]

            A_below_limit = protA_values [protA_values  < proteins_values[protA,3]]
            A_above_limit = protA_values [protA_values  > proteins_values[protA,4]]
            if ((length(A_below_limit) >= (n_signif_samples*0.5)) || (length(A_above_limit) >= (n_signif_samples*0.5)))
            {
                signif_indiv_prots[[protA]][i] = sum (abs(values_z_scores[signif_samples_low]), na.rm = T)
            }

            B_below_limit = protB_values [protB_values  < proteins_values[protB,3]]
            B_above_limit = protB_values [protB_values  > proteins_values[protB,4]]
            if ((length(B_below_limit) >= (n_signif_samples*0.5)) || (length (B_above_limit) >= (n_signif_samples*0.5)))
            {
                signif_indiv_prots[[protB]][i] = sum (abs(values_z_scores[signif_samples_low]), na.rm = T)
            }
        }
    }
    #print ("...")
    ### Find representative pairs. This step makes sure that in instances where one protein is strongly up-, down-regulated, not all of its pairs are reported.
    ### For each protein that contributed to the perturbation signal in the samples with significant z scores, find the pair with the strongest overall signal, i.e. maximum sum of significant z-scores.
    representative_pairs = data.frame()
    row = 1
    for (prot in names(signif_indiv_prots))
    {
        repres_pair = which.max (signif_indiv_prots[[prot]])  ### should somehow point to the most significant, i.e. representative protein pair
        name_repres_pair = names (repres_pair)[1]
        element = repres_pair[[1]]
        highest_score = signif_indiv_prots[[prot]][element]

        values_z_scores = as.numeric(modif_z_score[name_repres_pair,])

        if ((name_repres_pair %in% n_h) && (name_repres_pair %in% n_l)) {
            signif_samples_h  = which(values_z_scores > score_threshold)
            signif_samples_l  = which(values_z_scores < thresh_neg)
            signif_samples = unique(c(signif_samples_h, signif_samples_l))
            direction = "Both_directions"
        } else if (name_repres_pair %in% n_h) {
            signif_samples  = which(values_z_scores > score_threshold)
            direction = "Increased_ratio"
        } else if (name_repres_pair %in% n_l)	{
            signif_samples  = which(values_z_scores < thresh_neg)
            direction = "Decreased_ratio"
        }

        cols_anal_data = colnames (quant_data_disease)
        signif_cls = cols_anal_data[signif_samples]

        indiv_prots <- unlist (strsplit(name_repres_pair, "-"))
        protA = indiv_prots[1]
        protB = indiv_prots[2]
        if (!(is.null(signif_indiv_prots[[protA]])) && !(is.null(signif_indiv_prots[[protB]]))) {
            signal_contribution = paste (protA, protB, sep = "&")
        } else if (!(is.null(signif_indiv_prots[[protA]]))) {
            signal_contribution = protA
        } else if (!(is.null(signif_indiv_prots[[protB]]))) {
            signal_contribution = protB
        }

        #sign_sampl = paste (signif_samples, collapse = ",") OR, to get samples names and not column numbers:
        sign_sampl = paste (signif_cls, collapse = ",")
        representative_pairs[row, 1] = name_repres_pair
        representative_pairs[row, 2] = highest_score
        representative_pairs[row, 3] = sign_sampl
        representative_pairs[row, 4] = direction
        representative_pairs[row, 5] = signal_contribution
        #representative_pairs[row, 6] = results_table[name_repres_pair,1]
        #representative_pairs[row, 7] = results_table[name_repres_pair,2]
        row = row + 1
    }
    colnames (representative_pairs) = c("Protein_pair", "Score", "Significant_samples", "Change", "Signal_contribution") # , "Median_ref_set", "MAD_ref_set"
    representative_pairs = unique(representative_pairs)
    representative_pairs = representative_pairs[order(representative_pairs[,2], decreasing = T),]
    #representative_pairs = representative_pairs[representative_pairs[,6] != 0,]
    rows_to_stay = which (representative_pairs[,1] %in% names(measured_samples))
    representative_pairs = representative_pairs[rows_to_stay,]

    if (filter_variable_in_ref_set == "YES") {
    ##representative_pairs = representative_pairs [!(representative_pairs[,1] %in% var_pairs),]
    }

    if (write_table == "YES") {
        write.table(file = "Representative_pairs.txt", representative_pairs, quote = F, row.names = F, sep = "\t")
    }

    if (print_recomm == "YES") {
        #### ACCOUNT FOR ALL OBSERVED MODIF Z-SCORES, I.E. STANDARDIZED DISTANCES FROM THE REF POPULATION
        all_values = as.vector(as.matrix (modif_z_score))
        q1 = quantile (all_values, 0.999, na.rm = T)
        q2 = quantile (all_values, 0.99, na.rm = T)
        q3 = quantile (all_values, 0.95, na.rm = T)
        q4 = quantile (all_values, 0.001, na.rm = T)
        q5 = quantile (all_values, 0.01, na.rm = T)
        q6 = quantile (all_values, 0.05, na.rm = T)
        all_values_abs = as.vector(as.matrix (abs(modif_z_score)))
        q_abs = quantile (all_values_abs, 0.99, na.rm = T)
        q1 = round (q1, 2)
        q2 = round (q2, 2)
        q3 = round (q3, 2)
        q4 = round (q4, 2)
        q5 = round (q5, 2)
        q6 = round (q6, 2)
        message = paste ("Top 0.1, 1 and 5% upper and lower z-score values are:", q1, q2, q3, "and", q4, q5, q6, sep = " ")
        message = paste (message, ".", sep = "")
        print (message)
        q_abs = round (q_abs, 2)
        message_two = paste ("Top 1% of the absolute values for the modified z-scores is", q_abs, sep = " ")
        message_two = paste (message_two, ".", sep = "")
        print (message_two)
        plot (density(all_values, na.rm = T))
    }

    return (representative_pairs)
    #return(list(modif_z_scores = modif_z_score, RepresentativePairs = representative_pairs))
}
