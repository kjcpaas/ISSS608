convert_edges_to_resource_flow <- function(edges) {
  # Employee -> Employer, weight: 2
  works_for <- edges %>% filter(subtype == "WorksFor") %>% mutate(weight = 2)
  
  # Person1 <-> Person2, weight: 1
  family <- edges %>% filter(subtype == "FamilyRelationship")
  family_rev <- family %>%
    mutate(temp = from, from = to, to = temp) %>%
    select(from, to, supertype, subtype, weight)
  
  # Shareholder <- Company, weight: 2
  shareholder <- edges %>% filter(subtype == "Shareholdership") %>%
    mutate(temp = from, from = to, to = temp, weight = 2) %>%
    select(from, to, supertype, subtype, weight)
  
  # BeneficialOwner <- Company, weight: 3
  owner <- edges %>% filter(subtype == "BeneficialOwnership") %>%
    mutate(temp = from, from = to, to = temp, weight = 3) %>%
    select(from, to, supertype, subtype, weight)
  
  works_for %>%
    rbind(family) %>%
    rbind(family_rev) %>%
    rbind(shareholder) %>%
    rbind(owner)
}