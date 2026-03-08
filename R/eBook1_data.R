# Robust downloader for OctoberCMS Media HTML page
# - Downloads allowed files (overwrite) into working directory (getwd())
# - Extracts .gz and .zip (overwrite extracted targets) unless special
# - Special archives stay compressed and go to getwd(): eBook1.zip, eBook1.tar.gz
# - Does NOT add pdf/htm/Rmd

index_url <- "http://georges.biomatix.org/storage/app/media/eBook%20Introduction%20to%20dartR/Media%20_%20OctoberCMS.htm"
base_url  <- "http://georges.biomatix.org/storage/app/media/eBook%20Introduction%20to%20dartR/"

options(timeout = 300)

base_dir <- getwd()

# >>> CHANGE: download data files to working directory (not ./data)
data_dir <- base_dir

# Allowed extensions ONLY (no pdf/htm/Rmd)
allowed_ext <- c("xlsx","xls","csv","dat","txt","rdata","vcf","gz","zip")

# Special archives: keep compressed; download to base_dir
special_files <- c("eBook1.zip", "eBook1.tar.gz")
special_files_lc <- tolower(special_files)

# Files you want even if the HTML does not reference them
manual_add <- c("eBook_Exercise_4-1.Rdata", "eBook_Exercise_4-2.Rdata",
                special_files)

# Delete archives after extraction (non-special only)
delete_gz_after  <- TRUE
delete_zip_after <- TRUE

# ---------------- helpers ----------------

fetch_html <- function(url) {
  con <- url(url, open = "rb")
  on.exit(close(con), add = TRUE)
  raw <- readBin(con, what = "raw", n = 5e7)
  rawToChar(raw)
}

html_entity_decode <- function(x) {
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  x
}

clean_token <- function(x) {
  x <- html_entity_decode(x)
  x <- trimws(x)
  x <- gsub('^[\'"\\(\\[]+|[\'"\\)\\],;]+$', "", x)
  x <- sub("[?#].*$", "", x)                # drop query/fragment
  x <- utils::URLdecode(x)                  # decode %20 etc.
  trimws(x)
}

get_ext <- function(p) {
  bn <- basename(p)
  if (!grepl("\\.", bn)) return("")
  tolower(sub("^.*\\.", "", bn))
}

# Extract candidates from href/src/url(...) and from filename-like tokens
extract_candidates <- function(html) {
  m1 <- gregexpr("(?i)(?:href|src)\\s*=\\s*(['\"])(.*?)\\1", html, perl = TRUE)
  hits1 <- regmatches(html, m1)[[1]]
  urls1 <- if (length(hits1)) sub("(?i)(?:href|src)\\s*=\\s*(['\"])(.*?)\\1", "\\2", hits1, perl = TRUE) else character(0)

  m2 <- gregexpr("(?i)url\\(([^)]+)\\)", html, perl = TRUE)
  hits2 <- regmatches(html, m2)[[1]]
  urls2 <- if (length(hits2)) sub("(?i)url\\(([^)]+)\\)", "\\1", hits2, perl = TRUE) else character(0)

  ext_group <- "(?:xlsx|xls|csv|dat|txt|Rdata|rdata|vcf|gz|zip)"
  rx <- paste0("([A-Za-z0-9% _\\-./]+\\.", ext_group, ")(?:\\?[^\"'<> ]*)?")
  m3 <- gregexpr(rx, html, ignore.case = TRUE, perl = TRUE)
  hits3 <- regmatches(html, m3)[[1]]

  unique(c(urls1, urls2, hits3))
}

# Build a small ordered set of URL candidates for a given raw token
build_url_candidates <- function(token, base_url) {
  tok <- clean_token(token)
  if (!nzchar(tok)) return(character(0))

  base_dec <- utils::URLdecode(base_url)
  host <- sub("(https?://[^/]+).*", "\\1", base_dec)
  base_path_dec <- sub("^https?://[^/]+/?", "", base_dec)
  base_path_dec <- sub("^/+", "", base_path_dec)

  folder_name <- basename(dirname(base_dec))
  folder_prefix1 <- paste0(folder_name, "/")
  folder_prefix2 <- paste0("storage/app/media/", folder_name, "/")

  tok2 <- tok
  if (grepl("^https?://", tok2, ignore.case = TRUE)) {
    abs1 <- tok2
    path_only <- sub("^https?://[^/]+/?", "", tok2)
    tok2 <- path_only
  } else {
    abs1 <- NA_character_
  }

  tok2 <- sub("^/+", "", tok2)
  tok2 <- sub("^\\./+", "", tok2)

  tok3 <- tok2
  if (startsWith(tok3, folder_prefix2)) tok3 <- substr(tok3, nchar(folder_prefix2) + 1L, nchar(tok3))
  if (startsWith(tok3, folder_prefix1)) tok3 <- substr(tok3, nchar(folder_prefix1) + 1L, nchar(tok3))
  if (startsWith(tok3, base_path_dec))  tok3 <- substr(tok3, nchar(base_path_dec) + 1L, nchar(tok3))

  tok3 <- sub("^/+", "", tok3)

  encode_rel <- function(rel) {
    parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    parts <- vapply(parts, utils::URLencode, character(1), reserved = TRUE)
    paste(parts, collapse = "/")
  }

  bn <- basename(tok3)
  cand <- character(0)

  if (!is.na(abs1)) cand <- c(cand, abs1)
  if (nzchar(tok3))  cand <- c(cand, paste0(base_url, encode_rel(tok3)))
  if (nzchar(bn))    cand <- c(cand, paste0(base_url, utils::URLencode(bn, reserved = TRUE)))

  if (grepl("^storage/app/media/", tok2, ignore.case = TRUE)) {
    cand <- c(cand, paste0(host, "/", encode_rel(tok2)))
  }
  if (grepl("^/storage/app/media/", tok, ignore.case = TRUE)) {
    tok_slash <- sub("^/+", "", tok)
    cand <- c(cand, paste0(host, "/", encode_rel(tok_slash)))
  }

  unique(cand)
}

# --- gunzip helper (no packages) ---
gunzip_file <- function(gz_path, overwrite = TRUE, remove_gz = FALSE) {
  if (!grepl("\\.gz$", gz_path, ignore.case = TRUE)) return(invisible(NULL))
  out_path <- sub("\\.gz$", "", gz_path, ignore.case = TRUE)
  if (file.exists(out_path) && overwrite) file.remove(out_path)

  in_con  <- gzfile(gz_path, open = "rb")
  on.exit(close(in_con), add = TRUE)
  out_con <- file(out_path, open = "wb")
  on.exit(close(out_con), add = TRUE)

  repeat {
    buf <- readBin(in_con, what = "raw", n = 1024 * 1024)
    if (length(buf) == 0) break
    writeBin(buf, out_con)
  }

  if (remove_gz) file.remove(gz_path)
  out_path
}

# --- unzip helper (overwrite extracted files) ---
unzip_overwrite <- function(zip_path, exdir, remove_zip = FALSE) {
  members <- tryCatch(utils::unzip(zip_path, list = TRUE), error = function(e) NULL)
  if (is.null(members) || !"Name" %in% names(members)) {
    stop("Could not list contents of zip: ", basename(zip_path))
  }

  targets <- file.path(exdir, members$Name)
  exists <- file.exists(targets)
  if (any(exists)) suppressWarnings(file.remove(targets[exists]))

  utils::unzip(zipfile = zip_path, exdir = exdir)

  if (remove_zip) file.remove(zip_path)
  invisible(members$Name)
}

download_with_fallback <- function(fname, url_candidates, dest_dir, is_special = FALSE) {
  dest <- file.path(dest_dir, fname)

  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  if (file.exists(dest)) file.remove(dest)

  last_err <- NULL
  for (u in url_candidates) {
    ok <- tryCatch(
      {
        utils::download.file(u, destfile = dest, mode = "wb", quiet = TRUE)
        TRUE
      },
      error = function(e) {
        last_err <<- conditionMessage(e)
        FALSE
      }
    )
    if (ok && file.exists(dest) && file.info(dest)$size > 0) {
      cat("[ok]   ", fname, "\n", sep = "")
      if (!is_special) {
        if (grepl("\\.gz$", dest, ignore.case = TRUE)) {
          out_path <- gunzip_file(dest, overwrite = TRUE, remove_gz = delete_gz_after)
          cat("       gunzipped -> ", basename(out_path), "\n", sep = "")
        } else if (grepl("\\.zip$", dest, ignore.case = TRUE)) {
          extracted <- unzip_overwrite(dest, exdir = dest_dir, remove_zip = delete_zip_after)
          cat("       unzipped  -> ", length(extracted), " file(s)\n", sep = "")
        }
      } else {
        cat("       kept compressed (special file)\n")
      }
      return(TRUE)
    }
  }

  cat("[fail] ", fname, ": ", if (!is.null(last_err)) last_err else "all URL candidates failed", "\n", sep = "")
  FALSE
}

# ---------------- main ----------------

html <- fetch_html(index_url)

cands <- extract_candidates(html)
cands <- c(cands, manual_add)

cands_clean <- unique(vapply(cands, clean_token, character(1)))
cands_clean <- cands_clean[nzchar(cands_clean)]

exts <- vapply(cands_clean, get_ext, character(1))
cands_clean <- cands_clean[exts %in% allowed_ext]

if (length(cands_clean) == 0) stop("No candidates found with allowed extensions in HTML (even after manual_add).")

by_base <- split(cands_clean, tolower(basename(cands_clean)))

for (m in manual_add) {
  k <- tolower(basename(m))
  if (!k %in% names(by_base)) by_base[[k]] <- m
}

targets <- names(by_base)
cat("Target basenames to attempt: ", length(targets), "\n", sep = "")

ok_vec <- logical(length(targets))
names(ok_vec) <- targets

for (k in targets) {
  fname <- basename(by_base[[k]][1])

  is_special <- tolower(fname) %in% special_files_lc
  dest_dir   <- base_dir  # >>> CHANGE: everything goes to working dir

  variants <- unique(c(by_base[[k]], fname))
  url_candidates <- unique(unlist(lapply(variants, build_url_candidates, base_url = base_url)))

  ok_vec[k] <- download_with_fallback(fname, url_candidates, dest_dir, is_special = is_special)
}

cat("\nDone. OK:", sum(ok_vec), " Failed:", sum(!ok_vec), "\n", sep = "")
cat("Saved data under: ", normalizePath(base_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("Saved special archives under: ", normalizePath(base_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")

# >>> CHANGE: manifest written to working directory
manifest_path <- file.path(base_dir, "manifest.txt")
writeLines(names(ok_vec), manifest_path)
cat("Manifest written to: ", normalizePath(manifest_path, winslash = "/", mustWork = FALSE), "\n", sep = "")
