# R/postprocess_code_highlighting.R
#
# Quarto post-render hook. Adds three visual enhancements to compiled slide
# HTML that Pandoc's skylighting engine cannot produce on its own, because
# they require either (a) inventing a decoration skylighting has no token
# class for, or (b) tracking state across the whole code block rather than
# per token:
#
#   1. Hex color literals ("#F2777A") get rendered as a filled, rounded
#      "pill" in that exact color, with automatically contrasting text
#      (dark text on light colors, light text on dark colors).
#   2. Parentheses / brackets / braces are colored by nesting depth,
#      cycling through the same 7-color palette as the
#      editorBracketHighlight.foreground1-7 settings in
#      tomorrow-night-eighties-r-classic.json.
#   3. R namespace prefixes in `pkg::fn` calls get their own color
#      (entity.name.namespace.r in the VS Code theme), since skylighting
#      only tokenizes the `::` operator and the function name, not the
#      package name.
#
# This runs once per rendered .html file, AFTER Quarto/Pandoc have already
# produced final skylighting-tokenized output. It never touches source
# .qmd files and never runs during the Pandoc AST stage, which is why this
# is a post-render script rather than a Lua filter -- Lua filters run
# before syntax highlighting is applied, so they cannot see or edit
# individual highlighted tokens (span.fu, span.op, etc.).
#
# Requires: xml2
#
# Wire it up in _quarto.yml:
#
#   project:
#     post-render: R/postprocess_code_highlighting.R
#
# Quarto runs this script with Rscript after every render (including
# `quarto preview`). It receives no file arguments -- it discovers the
# rendered .html files itself, which is why every run starts from fresh
# Pandoc output rather than re-processing its own previous output.

library(xml2)

# ---- Palettes -------------------------------------------------------------
# Keep these in sync by hand with tomorrow-night-eighties-r-classic.json.
# If you build the automated JSON -> SCSS generator discussed earlier,
# these same seven values should be read from that JSON instead of
# hardcoded here.

bracket_colors <- c(
  "#ed90a4",
  "#d3a263",
  "#99b657",
  "#33c192",
  "#00bdce",
  "#94a9ec",
  "#dc91db"
)

namespace_color <- "#FFCC66"

hex_pattern <- "#(?:[0-9A-Fa-f]{3}){1,2}\\b"

open_brackets <- c("(", "[", "{")
close_brackets <- c(")", "]", "}")

# ---- Small helpers ----------------------------------------------------

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# WCAG relative luminance -> pick black or white pill text for contrast
contrasting_text_color <- function(hex) {
  hex <- sub("^#", "", hex)
  if (nchar(hex) == 3) {
    hex <- paste0(rep(strsplit(hex, "")[[1]], each = 2), collapse = "")
  }
  channels <- strtoi(substring(hex, c(1, 3, 5), c(2, 4, 6)), base = 16L) / 255
  linear <- ifelse(
    channels <= 0.03928,
    channels / 12.92,
    ((channels + 0.055) / 1.055)^2.4
  )
  luminance <- sum(linear * c(0.2126, 0.7152, 0.0722))
  if (luminance > 0.5) "#1a1a1a" else "#ffffff"
}

replace_node_with_html <- function(node, html) {
  # Wrapping in <span> keeps this a single well-formed fragment to parse,
  # even when the replacement is multiple sibling nodes.
  new_nodes <- read_xml(paste0("<span>", html, "</span>"), options = "RECOVER")
  xml_replace(node, new_nodes)
}

# ---- 1. Hex color pills ----------------------------------------------

wrap_hex_matches <- function(text_node) {
  txt <- xml_text(text_node)
  if (!grepl(hex_pattern, txt, perl = TRUE)) {
    return(invisible())
  }

  match_pos <- gregexpr(hex_pattern, txt, perl = TRUE)[[1]]
  if (match_pos[1] == -1) {
    return(invisible())
  }
  match_len <- attr(match_pos, "match.length")

  pieces <- character(0)
  cursor <- 1L
  for (i in seq_along(match_pos)) {
    start <- match_pos[i]
    len <- match_len[i]
    if (start > cursor) {
      pieces <- c(pieces, escape_html(substr(txt, cursor, start - 1)))
    }
    hex <- substr(txt, start, start + len - 1)
    fg <- contrasting_text_color(hex)
    pieces <- c(
      pieces,
      sprintf(
        '<span class="hex-pill" style="background-color:%s;color:%s;">%s</span>',
        hex,
        fg,
        hex
      )
    )
    cursor <- start + len
  }
  if (cursor <= nchar(txt)) {
    pieces <- c(pieces, escape_html(substr(txt, cursor, nchar(txt))))
  }

  replace_node_with_html(text_node, paste(pieces, collapse = ""))
}

add_hex_pills <- function(code_node) {
  # Fresh lookup each call -- do not cache across steps, since earlier
  # steps (namespace coloring) may have already replaced some nodes.
  text_nodes <- xml_find_all(code_node, ".//text()")
  for (tn in text_nodes) {
    wrap_hex_matches(tn)
  }
}

# ---- 2. Rainbow brackets -----------------------------------------------

add_rainbow_brackets <- function(code_node) {
  text_nodes <- xml_find_all(code_node, ".//text()")
  depth <- 0L
  n_colors <- length(bracket_colors)

  for (tn in text_nodes) {
    txt <- xml_text(tn)
    if (!grepl("[()\\[\\]{}]", txt, perl = TRUE)) {
      next
    }

    chars <- strsplit(txt, "")[[1]]
    out <- character(length(chars))

    for (i in seq_along(chars)) {
      ch <- chars[i]
      if (ch %in% open_brackets) {
        depth <- depth + 1L
        color <- bracket_colors[((depth - 1L) %% n_colors) + 1L]
        out[i] <- sprintf(
          '<span class="bracket-depth" style="color:%s;">%s</span>',
          color,
          ch
        )
      } else if (ch %in% close_brackets) {
        color <- bracket_colors[((max(depth, 1L) - 1L) %% n_colors) + 1L]
        out[i] <- sprintf(
          '<span class="bracket-depth" style="color:%s;">%s</span>',
          color,
          ch
        )
        depth <- max(depth - 1L, 0L)
      } else {
        out[i] <- escape_html(ch)
      }
    }

    replace_node_with_html(tn, paste(out, collapse = ""))
  }
}

# ---- 3. R namespace coloring (pkg::fn) ---------------------------------

add_namespace_coloring <- function(code_node) {
  # Only meaningful for R chunks -- caller restricts to code.sourceCode.r.
  sc_nodes <- xml_find_all(code_node, ".//span[@class='sc' and text()='::']")

  for (sc in sc_nodes) {
    prev <- xml_find_first(sc, "preceding-sibling::text()[1]")
    if (is.na(prev)) {
      next
    }

    txt <- xml_text(prev)
    m <- regmatches(txt, regexpr("[[:alnum:]._]+\\s*$", txt))
    if (length(m) == 0 || identical(m, "")) {
      next
    }

    pkg_with_ws <- m
    pkg <- trimws(pkg_with_ws)
    prefix_end <- nchar(txt) - nchar(pkg_with_ws)
    prefix <- if (prefix_end > 0) substr(txt, 1, prefix_end) else ""
    trailing_ws <- substr(pkg_with_ws, nchar(pkg) + 1, nchar(pkg_with_ws))

    replacement <- sprintf(
      '%s<span class="namespace" style="color:%s;">%s</span>%s',
      escape_html(prefix),
      namespace_color,
      escape_html(pkg),
      trailing_ws
    )
    replace_node_with_html(prev, replacement)
  }
}

# ---- Orchestration -------------------------------------------------------

process_code_block <- function(code_node, is_r) {
  if (is_r) {
    add_namespace_coloring(code_node)
  } # must run first: needs
  # original span.sc text
  # nodes still intact
  add_hex_pills(code_node) # then hex literals
  add_rainbow_brackets(code_node) # brackets last: depth must
  # be tracked across whatever
  # text nodes remain
}

process_html_file <- function(path) {
  doc <- tryCatch(read_html(path), error = function(e) NULL)
  if (is.null(doc)) {
    return(invisible())
  }

  code_blocks <- xml_find_all(doc, "//code[contains(@class,'sourceCode')]")
  if (length(code_blocks) == 0) {
    return(invisible())
  }

  for (code_node in code_blocks) {
    classes <- xml_attr(code_node, "class")
    is_r <- grepl("\\br\\b", classes)
    process_code_block(code_node, is_r)
  }

  write_html(doc, path, options = "format")
}

html_files <- list.files(
  ".",
  pattern = "\\.html$",
  recursive = TRUE,
  full.names = TRUE
)
invisible(lapply(html_files, process_html_file))
