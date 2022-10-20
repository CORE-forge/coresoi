#' Download and extract zip from target url
#' @description  Download and extract zip from target url
#' @keywords internal
#' @export
download_and_unzip <- function(zip_url) {
  temp <- tempdir()
  dir <- here("data-raw")
  zip_file <- basename(zip_url)
  zip_combine <- paste(temp, zip_file, sep = "/")
  logger::log_info("Start downloading 🗂 zip file...  ")
  download.file(zip_url, destfile = zip_combine)
  logger::log_info("Downloading completed ✅ ")
  logger::log_info("Start unzippin 📦 ...  ")
  unzip(zip_combine, exdir = dir, overwrite = TRUE)
  logger::log_info("Unzippin completed ✅ ")
  unlink(temp)
}
