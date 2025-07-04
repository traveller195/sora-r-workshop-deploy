% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sora_available.R
\name{sora_available}
\alias{sora_available}
\alias{sora_services}
\alias{sora_key_status}
\title{Is SoRa available?}
\usage{
sora_available(services = NULL, api_key = NULL)

sora_services(api_key = NULL)

sora_key_status(api_key = NULL)
}
\arguments{
\item{services}{If provided, checks if the specified services are
available. If \code{NULL} (default), checks if any service is available.}

\item{api_key}{A valid SoRa API key. Required to communicate with the SoRa
API. You can request a key by filling out
\href{https://sora.gesis.org/unofficial/sora-user-mod/users/request-api-key}{this form}
on the SoRa website.

If \code{NULL}, reads a key from the \code{SORA_API_KEY}
environment variable.

If "ask" (and the session is interactive), opens a masked prompt where you
can enter your key. The entered key is persistently stored in an environment
variable and can be used across sessions.

Please note that you are advised not to enter your key directly using this
argument. If you do, please make sure that you do not publish any code files
containing the key in plain text. Failure to do so can compromise your
access to the SoRa API.}
}
\value{
\itemize{
\item{\code{sora_available} returns \code{TRUE} or \code{FALSE}}
\item{\code{sora_services} returns a named list of class
\code{sora_services} containing information about available and unavailable
services.}
\item{\code{sora_key_status} returns a named list of class
\code{sora_key_status.}}
}
}
\description{
Checks whether all of the required SoRa components are up and
working and whether your API key is authorized. \code{sora_available}
always returns \code{TRUE} or \code{FALSE}. \code{sora_services} returns
a list of available and unavailable services. \code{sora_key_status} returns
a list containing details on the status of the API key.
}
\examples{
\dontshow{if (isTRUE(Sys.getenv("SORA_RUN_EXAMPLES")) && sora_available()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Checks whether the SoRa API can be accessed
sora_available()

# See what service providers are available
sora_services()

# Get details about a provided API key
sora_key_status()

# These functions can be used to create basic control flows
try(stopifnot(sora_available()))
try(stopifnot("IOER" \%in\% sora_services()$available))
try(stopifnot("ALLOW" \%in\% sora_key_status()$decision))
\dontshow{\}) # examplesIf}
}
