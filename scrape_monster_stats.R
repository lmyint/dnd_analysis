library(rvest)
library(stringr)
library(tibble)
library(RSelenium)

page <- read_html("https://www.dndbeyond.com/monsters")
num_pages <- page %>%
	html_nodes(".b-pagination-item") %>%
	html_text() %>%
	as.integer() %>%
	max(na.rm = TRUE)

#' Get links to individual monster pages
#' 
#' A list of all monsters is available at https://www.dndbeyond.com/monsters.
#' This function extracts the URLs for individual monster pages
#' that are present on a single page of search results.
#' 
#' @param url The URL for a single page of results
#' @return A character vector of links to individual monster pages
get_monster_links <- function(url) {
	page <- read_html(url)
	rel_links <- page %>%
		html_nodes(".link") %>%
		html_attr(name = "href")
	keep <- str_detect(rel_links, "/monsters/")
	rel_links <- rel_links[keep]
	abs_links <- paste0("https://www.dndbeyond.com", rel_links)
	abs_links
}

## Loop through all pages
all_monster_urls <- lapply(seq_len(num_pages), function(i) {
	url <- paste0("https://www.dndbeyond.com/monsters?page=", i)
	get_monster_links(url)
}) %>% unlist

################################################
## Functions for scraping monster information ##
################################################

#' Select text contained within a CSS selector
#' 
#' @param xml XML object returned by `read_html` or `html_nodes`
#' @param selector CSS selector string
#' @param trim Trim whitespace?
#' @return A character vector of selected text
select_text <- function(xml, selector, trim = TRUE) {
	text <- xml %>% 
		html_nodes(selector) %>%
		html_text
	if (trim) {
		text <- text %>%
			trimws
	}
	text
}

#' Replace empty text with some other text
#' 
#' @param text The original text to replace, if empty
#' @param to The replacement text
#' @return The (possibly) replaced text
replace_if_empty <- function(text, to) {
	if (length(text)==0) {
		text <- to
	}
	text
}

#' Scrape an individual monster's information page
#' 
#' @param url The URL of the monster's page
#' @return A `tibble` containing monster statistics and information
scrape_monster_page <- function(page) {
	## Monster name
	mon_name <- page %>% select_text(".page-title")
	## Metadata text: size, type, alignment
	meta_text <- page %>% select_text(".mon-stat-block__meta")
	sp <- str_split(meta_text, ",")[[1]]
	sp2 <- str_split(sp[1], " ")[[1]]
	mon_size <- sp2[1]
	mon_type <- sp2[2]
	mon_align <- trimws(sp[2])
	## Attributes: AC, HP, speed
	attr_nodes <- page %>%
		html_nodes(".mon-stat-block__attribute")
	attrs <- do.call(rbind, lapply(attr_nodes, function(node) {
		label <- node %>%
			select_text(".mon-stat-block__attribute-label")
		data_value <- node %>%
			select_text(".mon-stat-block__attribute-data-value")
		data_extra <- node %>%
			select_text(".mon-stat-block__attribute-data-extra") %>%
			replace_if_empty(NA)
		tibble(label = label, value = data_value, extra = data_extra)
	}))
	## Ability scores
	ability_block <- page %>%
		html_nodes(".ability-block__stat")
	ability_scores <- do.call(rbind, lapply(ability_block, function(node) {
		label <- node %>% select_text(".ability-block__heading")
		score <- node %>% select_text(".ability-block__score") %>% as.integer
		modifier <- node %>% select_text(".ability-block__modifier")
		tibble(label = label, score = score, modifier = modifier)
	}))
	## "Tidbits"
	## Saving Throws, Skills, Damage Immunities, Damage Vulnerabilities
	## Damage Resistances, Condition Immunities, Senses, Languages, CR
	tidbits <- page %>%
		html_nodes(".mon-stat-block__tidbit")
	tidbit_info <- do.call(rbind, lapply(tidbits, function(node) {
		label <- node %>% select_text(".mon-stat-block__tidbit-label")
		data <- node %>% select_text(".mon-stat-block__tidbit-data")
		tibble(label = label, data = data)
	}))
	## Description blocks
	## Misc Features, Actions, Legendary Actions
	descrip_blocks <- page %>%
		html_nodes(".mon-stat-block__description-block")
	descrip_block_info <- do.call(rbind, lapply(descrip_blocks, function(node) {
		heading <- node %>%
			select_text(".mon-stat-block__description-block-heading") %>%
			replace_if_empty("Misc Features")
		content <- node %>%
			select_text(".mon-stat-block__description-block-content p")
		tibble(label = heading, data = content)
	}))
	## More info: lore and extended description
	extended_info <- page %>% select_text(".more-info-content")
	## Tags
	mon_tags <- page %>% select_text(".monster-tag") %>% paste(collapse = ",") %>% replace_if_empty(NA)
	## Environment
	mon_env <- page %>% select_text(".environment-tag") %>% paste(collapse = ",") %>% replace_if_empty(NA)
	## Source
	mon_source <- page %>% select_text(".monster-source") %>% paste(collapse = ",") %>% replace_if_empty(NA)

	tibble(
		name = mon_name,
		size = mon_size,
		type = mon_type,
		alignment = mon_align,
		attributes = list(attrs),
		ability_scores = list(ability_scores),
		tidbits = list(tidbit_info),
		description = list(descrip_block_info),
		extra_info = extended_info,
		tags = mon_tags,
		environment = mon_env,
		source = mon_source
	)
}

rd <- rsDriver(browser = "chrome")
rem_dr <- rd[["client"]]

url <- "https://www.dndbeyond.com/login"
rem_dr$navigate(url)
rem_dr$findElement(using = "css selector", value = ".twitch-button")$clickElement()
## Manually enter username and password here
rem_dr$findElement(using = "css selector", value = ".js-authorize-text")$clickElement()

monster_info <- vector("list", length(all_monster_urls))
for (i in seq_along(all_monster_urls)) {
	url <- all_monster_urls[i]
	rem_dr$navigate(url)
	page <- read_html(rem_dr$getPageSource()[[1]])
	## If content has not been unlocked, the page will redirect
	curr_url <- rem_dr$getCurrentUrl()[[1]]
	if (curr_url == url) {
		monster_info[[i]] <- scrape_monster_page(page)
	} else {
		monster_info[[i]] <- NA
	}
	Sys.sleep(2)
	cat(i, " ")
}

monster_info <- do.call(rbind, monster_info)
saveRDS(monster_info, file = "objects/monster_info.rds")
