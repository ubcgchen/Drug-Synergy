get_sig <- function(drug_name) {
  # URL to query for a drug signature
  pert_url <- base::paste("https://api.clue.io/api/sigs?filter={\"where\":",
                        "{\"pert_iname\":\"",
                        drug_name,
                        "\"}}&user_key=",
                        config::get("api_key"),
                        sep = "")
  
  # Get the signature
  res <- httr::GET(pert_url) %>%
    httr::content(as = 'text') %>%
    jsonlite::fromJSON()
  
  return(res)
}

get_ref_drug_sig <- function(reference_drug) {
  
  if (is.null(reference_drug)) {
    index <- 0
    repeat {
      # Get signature for the drug that most reverses the disease signature
      index = index + 1
      reference_drug <- top_drugs$pert_iname[index]
      reference_drug_signature <- get_sig(reference_drug)
      
      if(is_empty(reference_drug_signature)) next
      
      # TODO: change to using a consensus signature (defaults to just the first signature right now)
      ref_up <- reference_drug_signature$up100_full[[1]]
      ref_down <- reference_drug_signature$dn100_full[[1]]
      
      break
    }
  } else {
    reference_drug_signature <- get_sig(reference_drug)
    if (is_empty(reference_drug_signature)) stop("no signature available,
                                                 please try a different drug")
  }
  
  ref_up <- reference_drug_signature$up100_full[[1]]
  ref_down <- reference_drug_signature$dn100_full[[1]]
  
  return (list(ref_up = ref_up, ref_down = ref_down, 
               reference_drug = reference_drug))
}

calculate_concordance_ratio <- function(drug_up, drug_dn, ref_up, ref_dn) {
  num_same_dir_as_ref_up <- length(Reduce(intersect, 
                                          list(drug_up, ref_up)))
  num_same_dir_as_ref_dn <- length(Reduce(intersect, 
                                          list(drug_dn,ref_down)))
  num_same_dir_as_ref <- num_same_dir_as_ref_up + num_same_dir_as_ref_dn
  
  num_diff_dir_as_ref_up_dn <- length(Reduce(intersect, 
                                             list(drug_up, ref_dn)))
  num_diff_dir_as_ref_dn_up <- length(Reduce(intersect, 
                                             list(drug_dn,ref_up)))
  num_diff_dir_as_ref <- num_diff_dir_as_ref_up_dn + num_diff_dir_as_ref_dn_up
  
  num_diff_dir_as_ref[num_diff_dir_as_ref==0] <- 1
  
  return(num_same_dir_as_ref/num_diff_dir_as_ref)
}

calculate_discordance_ratio <- function(drug_up, drug_dn, ref,
                                        disease_up, disease_dn) {
  
  different_up_dn <- Reduce(intersect, list(drug_up, disease_dn))

  num_different_up_dn_excluding_reference <-
    length(setdiff(different_up_dn, ref))
  
  different_dn_up <- Reduce(intersect, list(drug_dn, disease_up))

  num_different_dn_up_excluding_reference <-
    length(setdiff(different_dn_up, ref))
  
  num_diff_direction <- num_different_up_dn_excluding_reference + 
    num_different_dn_up_excluding_reference
  
  
  same_up <- Reduce(intersect, list(drug_up,disease_up))
  num_same_up_excluding_reference <-
    length(setdiff(same_up, ref))
  
  same_dn <- Reduce(intersect, list(drug_dn,disease_dn))
  num_same_dn_excluding_reference <-
    length(setdiff(same_dn, ref))

  num_same_direction <- num_same_up_excluding_reference + 
    num_same_dn_excluding_reference
  
  num_same_direction[num_same_direction == 0] <- 1
  return(num_diff_direction/num_same_direction)
}

calculate_orthogonality_score <- function(concordance_ratio, 
                                          discordance_ratio) {
  return(sqrt((1-concordance_ratio)^2+discordance_ratio^2))
}

normalize_scores <- function(scores) {
  scores$concordance <- unlist(map(scores$concordance, function(x) {
    unlist((x-min(scores$concordance))/(max(scores$concordance)-min(scores$concordance)))
  }))
  
  scores$discordance <- unlist(map(scores$discordance, function(x) {
    (x-min(scores$discordance))/(max(scores$discordance)-min(scores$discordance))
  }))
  
  return (scores)
}

insert_orthogonality_scores <- function(scores) {
  scores$ortho.score <- unlist(map2(scores$concordance, scores$discordance, 
                                   function(x, y) {
                                     (calculate_orthogonality_score(x, y))
                                   }))
  
  scores <- dplyr::arrange(scores, dplyr::desc(ortho.score)) %>% distinct()
  
  return(scores)
}

remove_na <- function(scores) {
  return(scores[complete.cases(scores), ])
}

get_synergistic_drug <- function(top_drugs, reference_drugs, ref_up, ref_down,
                                 positive_DEG, negative_DEG) {
  scores <- data.frame(Drug=rep(NA, nrow(top_drugs)), concordance=rep(NA, 1),
                       discordance=rep(NA, 2),
                       stringsAsFactors=FALSE)
  index <- 1
  for (drug in top_drugs$pert_iname) {
    
    # Skip the reference drug
    if (drug %in% reference_drugs) next
    
    # Get the drug signature
    drug_signature <- get_sig(drug)
    if (is_empty(drug_signature)) next
    
    # Calculate concordance ratio
    concordance_ratio <- calculate_concordance_ratio(drug_signature$up100_full[[1]],
                                                     drug_signature$dn100_full[[1]],
                                                     ref_up,
                                                     ref_down)
    
    # Calculate discordance ratio
    discordance_ratio <- calculate_discordance_ratio(drug_signature$up100_full[[1]],
                                                     drug_signature$dn_100[[1]],
                                                     Reduce(union, ref_up,
                                                            ref_down),
                                                     rownames(positive_DEG),
                                                     rownames(negative_DEG))
    
    # Add to new data to scores
    scores[index, 1] = drug
    scores[index, 2] = concordance_ratio
    scores[index, 3] = discordance_ratio
    
    index = index + 1
    
    if (index == 50) break
}
  
  # process scores and compute orthogonality scores
  scores <- 
    scores %>% 
    remove_na() %>%
    normalize_scores() %>% 
    insert_orthogonality_scores()
  
  return(scores[1,]$Drug)
}

synergize_drugs <- function(top_drugs, drug_name = NULL, positive_DEG, 
                            negative_DEG, combination_size) {

  drugs = c(drug_name)
  
  for (curr_drug_index in 1:combination_size) {
    ref_drug_sig <- get_ref_drug_sig(drugs[curr_drug_index])
    ref_up <- c(ref_up, ref_drug_sig$ref_up)
    ref_down <- c(ref_down, ref_drug_sig$ref_down)
    reference_drug <- ref_drug_sig$reference_drug
    
    if (is.null(drugs)) drugs <- c(drugs, reference_drug)
    
    synergistic_drug <- get_synergistic_drug(top_drugs, drugs, 
                                             ref_up, ref_down, 
                                             positive_DEG, negative_DEG)
    drugs = c(drugs, synergistic_drug)
    index = index + 1
  }
  
  print(drugs)
}
       