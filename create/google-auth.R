# Decrypt the deployment token; never fall back to interactive authentication.
if (!nzchar(Sys.getenv("ESTIMELEE_GOOGLE_KEY"))) {
  stop("Set ESTIMELEE_GOOGLE_KEY in the app's Connect Cloud variables.", call. = FALSE)
}
if (!file.exists("google-token.enc")) {
  stop("Missing google-token.enc. Run setup-google-auth.R locally first.", call. = FALSE)
}
estimelee_token <- tryCatch(
  gargle::secret_read_rds("google-token.enc", key = "ESTIMELEE_GOOGLE_KEY"),
  error = function(e) stop("Cannot decrypt the Google token. Check ESTIMELEE_GOOGLE_KEY.", call. = FALSE)
)
if (!inherits(estimelee_token, "Token2.0")) {
  stop("The decrypted file is not an OAuth token.", call. = FALSE)
}
googledrive::drive_auth(token = estimelee_token)
googlesheets4::gs4_auth(token = estimelee_token)
rm(estimelee_token)
