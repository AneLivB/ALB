install.packages("scholar")
library(scholar)
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("ggplot2")
library(ggplot2)

#Manual DOI / Link mapping table based on your publication list
manual_links <- c(
  # Theses
  "Effects of density on Antarctic fur seals" = "https://doi.org/10.4119/unibi/3017483",
  
  # Preprints / In-review
  "Advancing marine mammal conservation through omics tools and integration of Indigenous Ecological Knowledge" = "",
  "Immune signals dominate transcriptomic responses to developmental and life-history transitions in Antarctic fur seals" = "",
  "High population density limits predator access in Antarctic fur seal breeding colonies" = "https://doi.org/10.64898/2026.04.07.716769",
  
  # Peer-reviewed
  "Fine-scale spatiotemporal predator–prey interactions in an Antarctic fur seal colony" = "https://doi.org/10.1098/rsos.250931",
  "Exploring the interplay of epigenetics and individualization" = "https://doi.org/10.1016/j.tree.2025.12.010",
  "Individual variation in perceived density of conspecifics and its impacts on the realization of ecological niches" = "https://doi.org/10.1002/oik.11981",
  "How Can We Make Scientific Events More Inclusive? Insights From Q&A Sessions and Surveys From an International Conference" = "https://doi.org/10.1002/ece3.71588",
  "Sustainability in the laboratory: evaluating the reusability of microtitre plates for PCR and fragment detection" = "https://doi.org/10.1098/rsos.242226",
  "Little evidence of inbreeding depression for birth mass, survival and growth in Antarctic fur seal pups" = "https://doi.org/10.1038/s41598-024-62290-x",
  "Recognizing and marshalling the pre-publication error correction potential of open data for more reproducible science" = "https://doi.org/10.1038/s41559-023-02152-3"
)

# --- 1. FETCH DATA FROM SCHOLAR ---
scholar_id <- "bdCdKQEAAAAJ" 

all_pubs    <- get_publications(scholar_id)
cit_history <- get_citation_history(scholar_id)

# --- 2. CLASSIFY AND CATEGORIZE ---
# Identify thesis by title keyword
is_thesis <- str_detect(all_pubs$title, regex("Effects of density on Antarctic fur seals", ignore_case = TRUE))
all_pubs$journal[is_thesis] <- "PhD thesis"

# Identify preprints via journal/source field matching
is_preprint <- str_detect(all_pubs$journal, regex("biorxiv|medrxiv|arxiv|preprint|ecoevorxiv", ignore_case = TRUE)) & !is_thesis

thesis_df        <- all_pubs[is_thesis, ]
preprints_df     <- all_pubs[is_preprint, ]
peer_reviewed_df <- all_pubs[!is_preprint & !is_thesis, ]

# --- 3. GENERATE LISTS ---
clean_title_str <- function(x) {
  x <- tolower(x)
  x <- gsub("&amp;", "and", x)
  x <- gsub("&", "and", x)
  x <- gsub("[^a-z0-9]", "", x) # Removes all spaces, dashes, and colons
  return(x)
}

generate_pub_html <- function(df) {
  if (nrow(df) == 0) return("<p>None currently listed.</p>")
  
  df <- df[order(df$year, decreasing = TRUE), ]
  
  # Clean keys in manual_links once
  cleaned_manual_names <- clean_title_str(names(manual_links))
  
  html_items <- apply(df, 1, function(row) {
    authors <- row["author"]
    
    # Author formatting
    authors <- gsub("\\b([A-Z])([A-Z])\\b", "\\1.\\2.", authors)
    authors <- gsub("([A-Z])\\.\\s+([A-Z])\\b", "\\1.\\2.", authors)
    authors <- gsub("J\\.?\\s*I\\.?\\s*Hoffman", "J.I. Hoffman", authors, ignore.case = TRUE)
    authors <- gsub("(?<=[A-Z])\\s+(?=[A-Z])", ". ", authors, perl = TRUE)
    authors <- gsub("A\\.?\\s*L\\.?\\s*Berthelsen|Berthelsen,?\\s*A\\.?\\s*L\\.?", "**A.L. Berthelsen**", authors, ignore.case = TRUE)
    
    # Robust title matching
    pub_title_clean <- clean_title_str(row["title"])
    match_idx <- match(pub_title_clean, cleaned_manual_names)
    
    pub_url <- if (!is.na(match_idx)) unname(manual_links[match_idx]) else ""
    
    link_html <- if (pub_url != "") {
      display_text <- sub("https://doi.org/", "", pub_url)
      if (grepl("bioRxiv", pub_url, ignore.case = TRUE)) {
        sprintf(' (<a href="%s" target="_blank">bioRxiv</a>)', pub_url)
      } else {
        sprintf(' (doi: <a href="%s" target="_blank">%s</a>)', pub_url, display_text)
      }
    } else {
      ""
    }
    
    sprintf(
      "<li>%s (%s) “%s” <em>%s</em>.%s</li>",
      authors,
      row["year"],
      row["title"],
      row["journal"],
      link_html
    )
  })
  
  paste0("<ul>\n", paste(html_items, collapse = "\n"), "\n</ul>")
}

dir.create("assets/generated", showWarnings = FALSE, recursive = TRUE)

writeLines(generate_pub_html(peer_reviewed_df), "assets/generated/peer_reviewed_list.md")
writeLines(generate_pub_html(preprints_df),     "assets/generated/preprints_list.md")
writeLines(generate_pub_html(thesis_df),        "assets/generated/thesis_list.md")

# --- 4. PREPARE DATA & GENERATE DUAL-AXIS CHART ---
pubs_by_year <- peer_reviewed_df %>%
  group_by(year) %>%
  summarise(pubs = n(), .groups = "drop")

combined_data <- full_join(pubs_by_year, cit_history, by = "year") %>%
  filter(year > 2000 & year <= as.numeric(format(Sys.Date(), "%Y"))) %>%
  mutate(
    pubs = coalesce(pubs, 0),
    cites = coalesce(cites, 0)
  ) %>%
  arrange(year)

scale_factor <- max(combined_data$cites, na.rm = TRUE) / max(combined_data$pubs, na.rm = TRUE)

p_combined <- ggplot(combined_data, aes(x = factor(year))) +
  geom_col(aes(y = pubs, fill = "Publications"), width = 0.6) +
  geom_line(aes(y = cites / scale_factor, group = 1, color = "Citations"), linewidth = 1.2) +
  geom_point(aes(y = cites / scale_factor, color = "Citations"), size = 2) +
  scale_y_continuous(
    name = "Number of publications",
    labels = function(x) sprintf("%.0f", x),
    sec.axis = sec_axis(
      ~ . * scale_factor, 
      name = "Citations per year",
      labels = function(x) sprintf("%.0f", x)
    )
  ) +
  scale_fill_manual(values = c("Publications" = "#43695F")) + # Dark forest green
  scale_color_manual(values = c("Citations" = "black")) +    # Coral red
  labs(
    title = "Peer-reviewed publications & citations",
    x = "Year",
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.title.y.left = element_text(color = "#43695F", face = "bold"),
    axis.title.y.right = element_text(color = "black", face = "bold")
  )

ggsave("assets/images/pubs_and_cites.png", plot = p_combined, width = 8, height = 4.5, dpi = 300)

