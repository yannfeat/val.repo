#' Read FASTA file as character vector.
#'
#' @param fname name of file to be read.
#'
#' @return character vector with names corresponding to annotations from FASTA.
#' @export
read_fasta = function(fname) {
	fas = readLines(fname)
	dat = grep(">", fas, value=TRUE)
	
	seq_coords = cbind(grep(">", fas)+1, c(c(grep(">", fas)-1)[-1], length(fas)))
	seqs = apply(seq_coords, 1, function(x) paste(fas[x[1]:x[2]], collapse=""))
	names(seqs) = dat
	names(seqs) = gsub(">", "", sapply(strsplit(names(seqs), split="\\|"), '[[', 1))
	seqs
}

map_one_active_site = function(pos, residue, my_pep, gene, flank=7) {
	pos_end = pos+nchar(residue)-1
	if (!all(c(pos, pos_end) %in% 1:length(my_pep))) {
		cat("warning: Indicated signaling sites of ", pos, pos_end, "not present in gene", gene, "of length", length(my_pep), "; skipping\n")
		return(NULL)		
	}
	if (paste(my_pep[pos:pos_end], collapse="") != residue) {
		cat("warning: Indicated signaling sequence", residue, "not equal to wildtype sequence",  paste(my_pep[pos:pos_end], collapse="") ,"of", gene, "at ", pos, pos_end, "; skipping\n")
		return(NULL)
	}
	regions = rep(FALSE, length(my_pep))
	regions[intersect((pos-flank):(pos_end+flank), 1:length(my_pep))] = TRUE
	regions
}

map_all_active_sites = function(active_site, pep, gene, flank=7) {
	if (nrow(active_site)==0){
		return(NULL)	
	}
	my_pep = strsplit(pep, split="")[[1]]
	mat = do.call("rbind", lapply(1:nrow(active_site), function(i) map_one_active_site(active_site[i, "position"], as.character(active_site[i, "residue"]), my_pep, gene, flank=flank)))
	if (is.null(mat[[1]]) || nrow(mat)==0){
		return(NULL)	
	}
	active_site_reg = apply(mat, 2, any)
	
	active_site_rle_v = rle(active_site_reg)$values
	active_site_rle_p1 = cumsum(rle(active_site_reg)$lengths)
	active_site_rle_p0 = c(1, active_site_rle_p1[-length(active_site_rle_p1)]+1)

	counter = 1
	for (i in 1:length(active_site_rle_v))	{
		active_site_reg[active_site_rle_p0[i]:active_site_rle_p1[i]] = ifelse(active_site_rle_v[i], counter, 0)
		counter = ifelse(active_site_rle_v[i], counter+1, counter)
	}
	active_site_reg	
}

#pos = muts[i, "position"]; wt_residue = muts[i, "wt_residue"]; mut_residue = muts[i, "mut_residue"]; my_count = muts[i, "count"]; my_pep = strsplit(pep, split="")[[1]]
map_one_mutation = function(pos, wt_residue, mut_residue, pep, gene, skip_mismatch, my_count) {
	if (!pos %in% 1:length(pep)) {
		cat("warning: proposed mutated position", pos, "not present in gene", gene, "of length", length(pep), "; skipping\n")
		return(NULL)		
	}
	skip_word = ifelse(skip_mismatch, "", "NOT") 
	if (pep[[pos]] != wt_residue) {
		cat("warning: proposed wildtype residue", 
				wt_residue, 
				"not equal to expected wildtype residue", 
				pep[pos] ,"of", gene, "at position", 
				pos, "; ", skip_word," skipping\n")
		if (skip_mismatch) {
			return(NULL)
		}
	}
	return(rep(pos, my_count))
}

#muts=l$mutations_input; pep=l$protein_sequence; gene=l$gene; skip_mismatch=T
map_all_mutations = function(muts, pep, gene, skip_mismatch) {
	if (nrow(muts)==0){
		return(NULL)	
	}
	my_pep = strsplit(pep, split="")[[1]]
	posi = unlist(lapply(1:nrow(muts), function(i) map_one_mutation(muts[i, "position"], muts[i, "wt_residue"], muts[i, "mut_residue"], my_pep, gene, skip_mismatch=skip_mismatch, muts[i, "count"])))
	if (is.null(posi[[1]])) {
		return(NULL)
	}
	lapply(1:length(my_pep), function(x) which(x==posi))
}

get_mutation_status = function(pos, active_site_pos, flank, mid_flank) {
	if (is.null(active_site_pos[[1]])) {
		return("")
	}
	stat = c()
	if (active_site_pos[pos]>0) {	
		stat = c(stat, "DI")	
	}
	proximal_flank_reg = intersect(setdiff((pos-mid_flank):(pos+mid_flank), pos), 1:length(active_site_pos))
	if (flank>0 & any(active_site_pos[proximal_flank_reg]>0)) {	
		stat = c(stat, "N1")	
	}
	distal_flank_reg = intersect(setdiff((pos-flank):(pos+flank), proximal_flank_reg), 1:length(active_site_pos))
	if (flank>0 & any(active_site_pos[distal_flank_reg]>0)) {	
		stat = c(stat, "N2")	
	}
	paste(stat, collapse=",")
}

count_flanking_active_sites_in_sequence = function(pos, active_sites, f1, f2) {
	posi = c((pos-f2):(pos-f1), (pos+f1):(pos+f2))
	posi = intersect(posi, 1:length(active_sites))
	length(which(active_sites[posi]!=0))
}

glm1 = function(form, data, type="poisson") {
	if (type=="nb") {
		return(MASS::glm.nb(stats::as.formula(form), data=data))
	} else {
		return(stats::glm(stats::as.formula(form), family=stats::poisson, data=data))
	}
}


# r = 1; active_site_regions = l$active_regions; mut_pos = l$mutations_per_position; active_site_pos = l$active_sites_in_sequence; dis = l$disorder
assess_one_region = function(r, active_site_regions, mut_pos, active_site_pos, dis, flank, mid_flank, simplified=FALSE, type="poisson") {
	n_muts = sapply(mut_pos, length)
	rr = active_site_regions == r
	dfr = data.frame(rr, n_muts, dis=dis==1)
	h0 = try(glm1("n_muts~dis", data=dfr, type=type), T)
	if (simplified) {
		h1 = try(glm1("n_muts~rr+dis", data=dfr, type=type), T)
	} else {
		jp_sites_nearby = n2_sites_nearby = n1_sites_nearby = rep(0, length(active_site_pos))
		n2_sites_nearby[active_site_regions == r] = sapply(which(active_site_regions == r), count_flanking_active_sites_in_sequence, active_site_pos, mid_flank+1, flank)
		n1_sites_nearby[active_site_regions == r] = sapply(which(active_site_regions == r), count_flanking_active_sites_in_sequence, active_site_pos, 1, mid_flank)
		jp_sites_nearby[active_site_regions == r] = sapply(which(active_site_regions == r), count_flanking_active_sites_in_sequence, active_site_pos, 0, 0)
		dfr = data.frame(rr, n_muts, n2_sites_nearby, n1_sites_nearby, jp_sites_nearby, dis=dis==1)
		h1_scope = paste("n_muts~", paste(collapse="+", setdiff(colnames(dfr), c("n_muts"))))
		h1 = NULL
		if (type=="nb") {
			h1 = try(MASS::stepAIC(MASS::glm.nb(n_muts~dis, data=dfr), trace=0, direction="forward", scope=stats::as.formula(h1_scope)), T)
		} else {
			h1 = try(MASS::stepAIC(stats::glm(n_muts~dis, data=dfr, family=stats::poisson), trace=0, direction="forward", scope=stats::as.formula(h1_scope)), T)
		}
	}
	if (any(c(class(h0)[[1]], class(h1)[[1]])=="try-error")) {	
		return(data.frame(p=NA, low=NA, med=NA, high=NA, obs=NA, stringsAsFactors=FALSE))
	}
	p = stats::anova(h0, h1, test="Chisq")[2, ifelse(type=="nb", "Pr(Chi)", "Pr(>Chi)")]
	h0_predicted_lambdas = stats::predict(h0, type="response")[rr]
	if (type=="nb") {
		exp_sampled = replicate(1000, sum(MASS::rnegbin(length(h0_predicted_lambdas), mu=h0_predicted_lambdas, theta=h0$theta), na.rm=T))
	} else {
		exp_sampled = replicate(1000, sum(stats::rpois(n=length(h0_predicted_lambdas), lambda=h0_predicted_lambdas)))
	}
	exp_mean = mean(exp_sampled)
	exp_sd = stats::sd(exp_sampled)
	res = data.frame(p, low=exp_mean-exp_sd, med=exp_mean, high=exp_mean+exp_sd, obs=sum(n_muts[rr]), stringsAsFactors=FALSE)
	rm(h0,h1,dfr)
	gc()
	res
}

assess_all_regions = function(active_site_regions, mut_pos, active_site_pos, dis, flank, mid_flank, simplified=FALSE, all_sites_together=FALSE, type="poisson") {
	if (all_sites_together) {
		active_site_regions = (active_site_regions>0)+0
	}
	all_reg = setdiff(unique(active_site_regions), 0)
	data.frame(region=all_reg, do.call("rbind", lapply(all_reg, assess_one_region, active_site_regions, mut_pos, active_site_pos, dis, flank, mid_flank, simplified, type=type)), stringsAsFactors=FALSE)
}


create_gene_record = function(gene, fasta, mut, pho, disorder, flank=7, mid_flank=2, simplified=FALSE, all_sites_together=FALSE, skip_mismatch=TRUE, type="poisson", enriched_only=TRUE) {
	l = list()
	l$gene = gene
	l$protein_sequence = fasta[[gene]]
	l$disorder = rep(0, nchar(l$protein_sequence))
	if (!is.null(disorder[[gene]][[1]])) {
		l$disorder = as.numeric(strsplit(disorder[[gene]], split="")[[1]])
	}
	l$mutations_input = mut[mut$gene==gene,,drop=FALSE]
	l$mutations_input = l$mutations_input[l$mutations_input$position %in% 1:nchar(l$protein_sequence),,drop=F]
	l$active_sites_input = pho[pho$gene==gene,,drop=FALSE]
	l$active_sites_input = l$active_sites_input[l$active_sites_input$position %in% 1:nchar(l$protein_sequence),,drop=F]
	
	if (simplified) {	
		flank = mid_flank = 0
	}
	
	l$active_regions = map_all_active_sites(l$active_sites_input, l$protein_sequence, l$gene, flank=flank)
	l$active_sites_in_sequence = map_all_active_sites(l$active_sites_input, l$protein_sequence, l$gene, flank=0)
	l$mutations_per_position = map_all_mutations(l$mutations_input, l$protein_sequence, l$gene, skip_mismatch=skip_mismatch)
	if (nrow(l$mutations_input)>0) {	l$mutations_input$status = ""	}
	if (nrow(l$mutations_input)>0) {	l$mutations_input$active_region = 0	}
	if (nrow(l$active_sites_input)>0) {	l$active_sites_input$active_region = 0	}
	if (!is.null(l$mutations_per_position[[1]])) {
		l$mutations_input$status = sapply(l$mutations_input$position, get_mutation_status, l$active_sites_in_sequence, flank, mid_flank)
	}
	if (!is.null(l$active_sites_in_sequence[[1]])) {
		regi = unique(setdiff(unique(l$active_regions), 0))
		l$active_region_summary = 
			do.call(rbind, lapply(regi, function(i) data.frame(gene, reg=i, t(summary(which(l$active_regions==i))[c("Min.", "Max.")]))))
		colnames(l$active_region_summary)[3:4] = c("pos0", "pos1")
	}
	
	if (!is.null(l$mutations_per_position[[1]]) & !is.null(l$active_sites_in_sequence[[1]])) {	
		l$region_mutation_significance = 
			assess_all_regions(l$active_regions, l$mutations_per_position, l$active_sites_in_sequence, l$disorder, flank, mid_flank, simplified, all_sites_together, type=type)
		tot = prod(l$region_mutation_significance[l$region_mutation_significance$p<0.05, "p"], na.rm=T)
		if (enriched_only) {
			tot = prod(l$region_mutation_significance[l$region_mutation_significance$p<0.05 & l$region_mutation_significance$obs>l$region_mutation_significance$med, "p"], na.rm=T)
		}
		l$total_mutation_significance = data.frame(p=tot, fdr=tot)
		l$active_sites_input$active_region = l$active_regions[l$active_sites_input$position]	
		l$mutations_input$active_region = l$active_regions[l$mutations_input$position]
	} 
	cat(".")
	gc()
	l
}


merge_report = function(all_active_sites, all_active_regions, all_region_based_pval, all_active_mutations, flank) {	
	colnames(all_active_sites)[2] = "PTM_position"
	all_active_regions$actreg = paste(all_active_regions$gene, all_active_regions$reg, sep=":")
	all_region_based_pval$actreg = paste(all_region_based_pval$gene, all_region_based_pval$region, sep=":")
	all_active_sites$actreg = paste(all_active_sites$gene, all_active_sites$active_region, sep=":")
	all_active_mutations$actreg = paste(all_active_mutations$gene, all_active_mutations$active_region, sep=":")
	
	mat1 = merge(all_active_mutations, all_active_regions, by="actreg")
	mat2 = merge(mat1, all_active_sites, by="actreg")
 	colnames(all_region_based_pval)[colnames(all_region_based_pval)=="gene"] = "gene1"
 	mat3 = merge(mat2, all_region_based_pval, by="actreg")
 	
	exclude_cols = 
		c("actreg", "cancer_type", "gene.y", "reg", "pos0", "pos1", "gene.x", "active_region.y","gene.y","region", "low", "med", "high", "obs", "gene1")
	mat3 = mat3[,!colnames(mat3) %in% exclude_cols]
	colnames(mat3)[colnames(mat3)=="position"] = "mut_position"
	colnames(mat3)[colnames(mat3)=="active_region.x"] = "active_region"
	colnames(mat3)[colnames(mat3)=="p"] = "active_region_p"
	# exclude psites that are further away from mutations than +/- 7 
	# however keep all direct sites as all active regions are annotated as DI when simplified=T
	mat3 = mat3[mat3$status=="DI" | (mat3$PTM_position>=mat3$mut_position-flank & mat3$PTM_position<=mat3$mut_position+flank),]
	rownames(mat3) = NULL
	mat3
}


check_mutations_and_sites = function(seqs_to_check, muts_to_check, sites_to_check, genes_to_check) {
  genes <- character()
  for (i in 1:length(muts_to_check$gene)) {
    if (muts_to_check$gene[i] %in% genes_to_check & !(muts_to_check$gene[i] %in% genes)) {
      seq_to_check <- seqs_to_check[muts_to_check$gene[i]]
      position <- muts_to_check$position[i]
      if (muts_to_check$wt_residue[i] == substring(seq_to_check, position, position)) {
        genes <- append(genes, muts_to_check$gene[i])
      }
    }
  }
  
  for (i in 1:length(sites_to_check$gene)) {
    if (sites_to_check$gene[i] %in% genes) {
      seq_to_check <- seqs_to_check[sites_to_check$gene[i]]
      position <- sites_to_check$position[i]
      length <- nchar(sites_to_check$residue[i])
      if (sites_to_check$residue[i] == substring(seq_to_check, position, position + length - 1)) {
        return(TRUE)
      }
    }
  }
  
  FALSE
}


#' Identification of active protein sites (post-translational modification sites, signalling domains, etc) with
#'     specific and significant mutations. 
#'
#' @param sequences character vector of protein sequences, names are protein IDs.
#' @param seq_disorder character vector of disorder in protein sequences, names are protein IDs and values are strings
#'     1/0 for disordered/ordered protein residues.
#' @param mutations data frame of mutations, with [gene, sample_id, position, wt_residue, mut_residue] as columns.
#' @param active_sites data frame of active sites, with [gene, position, residue, kinase] as columns. Kinase field may
#'     be blank and is shown for informative purposes.
#' @param flank numeric for selecting region size around active sites considered important for site activity. Default
#'     value is 7. Ignored in case of simplified analysis.
#' @param mid_flank numeric for splitting flanking region size into proximal (<=X) and distal (>X). Default value is
#'     2. Ignored in case of simplified analysis.
#' @param mc.cores numeric for indicating number of computing cores dedicated to computation. Default value is 1.
#' @param simplified true/false for selecting simplified analysis. Default value is FALSE. If TRUE, no flanking regions
#'     are considered and only indicated sites are tested for mutations. 
#' @param return_records true/false for returning a collection of gene records with more data regarding sites and
#'     mutations. Default value is FALSE.
#' @param skip_mismatch true/false for skipping mutations whose reference protein residue does not match expected
#'     residue from FASTA sequence file. 
#' @param regression_type 'nb' for negative binomial, 'poisson' for poisson GLM. The latter is default.
#' @param enriched_only true/false to indicate whether only sites with enriched active site mutations will be included
#'     in the final p-value estimation (TRUE is default). If FALSE, sites with less than expected mutations will be also
#'     included.
#'
#' @return list with the following components: 
#' 	@return all_active_mutations - table with mutations that hit or flank an active site. Additional columns of
#'      interest include Status (DI - direct active mutation; N1 - proximal flanking mutation; N2 - distal flanking
#'      mutation) and Active_region (region ID of active sites in that protein).
#'	@return all_active_sites - 
#'	@return all_region_based_pval - p-values for regions of sites, statistics on observed mutations (obs) and expected
#'      mutations (exp, low, high based on mean and s.d. from Poisson sampling). The field Region identifies region in
#'      all_active_sites.
#	@return all_gene_based_fdr - gene-based uncorrected and FDR-corrected p-values aggregated over multiple sites.
#	@return gene_records - if return_records is TRUE, a list of gene-based records is returned (large size).
#' @references Systematic analysis of somatic mutations in phosphorylation signaling predicts novel cancer drivers
#'     (2013, Molecular Systems Biology) by Juri Reimand and Gary Bader.
#' @author  Juri Reimand <juri.reimand@@utoronto.ca>
#' @examples
#' data(ActiveDriver_data)
#' \donttest{
#' phos_results = ActiveDriver(sequences, sequence_disorder, mutations, phosphosites)
#' ovarian_mutations = mutations[grep("ovarian", mutations$sample_id),]
#' phos_results_ovarian = ActiveDriver(sequences, sequence_disorder, ovarian_mutations, phosphosites)
#' GBM_muts = mutations[grep("glioblastoma", mutations$sample_id),]
#' kin_rslt_GBM = ActiveDriver(sequences, sequence_disorder, GBM_muts, kinase_domains, simplified=TRUE)
#' }
#' kin_results = ActiveDriver(sequences, sequence_disorder, mutations, kinase_domains, simplified=TRUE)
#' @export

ActiveDriver = function(sequences, seq_disorder, mutations, active_sites, flank = 7, mid_flank = 2, mc.cores = 1, simplified = FALSE, return_records = FALSE, skip_mismatch = TRUE, regression_type = "poisson", enriched_only = TRUE) {
	
	# first initiate mutation counts if needed
	if(is.null(mutations$count)) {mutations$count=1}
	# then check that no counts are zero
	mutations = mutations[mutations$count>0,]
	
	# replace factors with characters
	mutations[] = lapply(mutations, as.character)
	active_sites[] = lapply(active_sites, as.character)
	# make sure positions are numeric
	mutations$position = as.numeric(mutations$position)
	active_sites$position = as.numeric(active_sites$position)
	
	# ensure case is uniform
	mutations$wt_residue = toupper(mutations$wt_residue)
	mutations$mut_residue = toupper(mutations$mut_residue)
	active_sites$residue = toupper(active_sites$residue)
	sequences = toupper(sequences)
	
	genes_to_test = Reduce(intersect, list(mutations$gene, active_sites$gene, names(sequences), names(seq_disorder)))
	if (length(genes_to_test)<1) { 
		cat("Error: no genes matched in tables for mutations, active sites and sequences\n"); 
		return(NULL)
	}
	# check if mutation and active site wt residues match with reference sequences
	if (check_mutations_and_sites(sequences, mutations, active_sites, genes_to_test) == FALSE) {
	  cat("Error: wildtype residues do not match reference sequences\n")
	  return(NULL)
	}
	
	cat("genes:", length(genes_to_test))
	gene_records = parallel::mclapply(genes_to_test, create_gene_record, 
			sequences, mutations, active_sites, seq_disorder, flank=flank, mid_flank=mid_flank, 
			mc.cores=mc.cores, simplified=simplified, skip_mismatch=skip_mismatch, type=regression_type, enriched_only=enriched_only, mc.preschedule=F)
	names(gene_records) = sapply(gene_records, '[[', 'gene')
	
	all_gene_based_fdr = do.call("rbind", 
		lapply(gene_records, function(gr) 
			if(!is.null(gr$total_mutation_significance[[1]])) 
				data.frame(gene=gr$gene, gr$total_mutation_significance, stringsAsFactors=FALSE)))
	if (!is.null(all_gene_based_fdr[[1]])) {
		all_gene_based_fdr$fdr = stats::p.adjust(all_gene_based_fdr$fdr, method="fdr")
		# update fdr values in gene records
		for (i in 1:nrow(all_gene_based_fdr)) {
			gene_records[[as.character(all_gene_based_fdr[i, "gene"])]]$total_mutation_significance[,"fdr"] = all_gene_based_fdr[i,"fdr"]
		}
	}	
	all_region_based_pval =  do.call("rbind", 
		lapply(gene_records, function(gr) 
			if(!is.null(gr$region_mutation_significance[[1]])) data.frame(gene=gr$gene, gr$region_mutation_significance, stringsAsFactors=FALSE)))
	all_active_mutations = do.call("rbind", lapply(gene_records, '[[', 'mutations_input'))
	all_active_mutations = all_active_mutations[all_active_mutations$status!="",]
	all_active_sites = do.call("rbind", lapply(gene_records, '[[', 'active_sites_input'))
	all_active_sites = all_active_sites[all_active_sites$active_region>0,,drop=F]
	all_active_regions = do.call("rbind", lapply(gene_records, '[[', 'active_region_summary'))
	rownames(all_active_mutations) = rownames(all_active_sites) = rownames(all_active_regions) = rownames(all_region_based_pval) = NULL
	## assemble merged report
	merged_report = merge_report(all_active_sites, all_active_regions, all_region_based_pval, all_active_mutations, flank)

	results = list(all_active_mutations, all_active_sites, all_region_based_pval, all_gene_based_fdr, all_active_regions, merged_report)
	names(results) = c("all_active_mutations", "all_active_sites", "all_region_based_pval", "all_gene_based_fdr", "all_active_regions", "merged_report")
	if (return_records) {
		results$gene_records = gene_records
	}
	rm(gene_records)
	gc()
	results
}


#' Example protein sequences for ActiveDriver
#'
#' A dataset containing the sequences of four proteins.
#'
#' @docType data
#' @keywords datasets
#' @name sequences
#' @usage data(ActiveDriver_data)
#' @format A named character vector with 4 elements
NULL

#' Example protein disorder for ActiveDriver
#'
#' A dataset containing the disorder of four proteins.
#'
#' @docType data
#' @keywords datasets
#' @name sequence_disorder
#' @usage data(ActiveDriver_data)
#' @format A named character vector with 4 elements
NULL

#' Example mutations for ActiveDriver
#'
#' A dataset describing mis-sense mutations (i.e., substitutions in proteins). The variables are as follows:
#'
#' \itemize{
#'   \item gene. the mutated gene
#'   \item sample_id. the sample where the mutation originates
#'   \item position. the position in the protein sequence where the mutation occurs
#'   \item wt_residue. the wild-type residue
#'   \item mut_residue. the mutant residue
#' }
#'
#' @docType data
#' @keywords datasets
#' @name mutations
#' @usage data(ActiveDriver_data)
#' @format A data frame with 408 observations of 5 variables
NULL

#' Example phosphosites for ActiveDriver
#'
#' A dataset describing protein phosphorylation sites. The variables are as follows:
#'
#' \itemize{
#'   \item gene. the gene symbol the phosphosite occurs in
#'   \item position. the position in the protein sequence where the phosphosite occurs
#'   \item residue. the phosphosite residue
#'   \item kinase. the kinase that phosphorylates this site
#' }
#'
#' @docType data
#' @keywords datasets
#' @name phosphosites
#' @usage data(ActiveDriver_data)
#' @format A data frame with 131 observations of 4 variables
NULL

#' Example kinase domains for ActiveDriver
#'
#' A dataset describing kinase domains. The variables are as follows:
#'
#' \itemize{
#'   \item gene. the gene symbol of the gene where the kinase domain occurs
#'   \item position. the position in the protein sequence where the kinase domain begins
#'   \item phos. TRUE
#'   \item residue. the kinase domain residues
#' }
#'
#' @docType data
#' @keywords datasets
#' @name kinase_domains
#' @usage data(ActiveDriver_data)
#' @format A data frame with 1 observation of 4 variables
NULL
