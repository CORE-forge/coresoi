#' BDNCP open data from ANAC
#'
#' available data can be consulted and downloaded through the
#' appropriate subsections; in particular, in the "Analytics" section
#' there is a dashboard for browsing and analyzing data on public contracts,
#' while in the "Dataset" section a series of freely downloadable
#' JSON and CSV and XML files are available.
#'
#' @format ## `who`
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{cig, cig_accordo_quadro}{Tender Identification Code, is a code assigned by the Authority to uniquely track tenders and contracts nationwide and is assigned at the Lot level.}
#'   \item{numero_gara,}{number identifying a tender; a tender consists of one or more lots, each identified by a CIG}
#'   \item{oggetto_gara}{Subject of the Tender - description of goods and services to be procured}
#'   \item{importo_complessivo_gara}{Overall Amount of the Tender}
#'   \item{n_lotti_componenti}{Number of lots that make up the contract}
#'   \item{oggetto_lotto}{Subject of the Lot}
#'   \item{importo_lotto}{Lot amount}
#'   \item{oggetto_principale_contratto}{Main object of the contract (Works, Services Supplies)}
#'   \item{stato}{cig life cycle status}
#'   \item{settore}{indicates whether the Contracting Authority operates in the ordinary sectors (EU Directive 24 applies) or in the Special Sectors (EU Directive 25 applies)}
#'   \item{luogo_istat}{ISTAT Localization}
#'   \item{provincia}{Province abbreviation}
#'   \item{data_pubblicazione}{date of publication of the notice}
#'   \item{data_scadenza_offerta}{Deadline for submission of a bid}
#'   \item{cod_tipo_scelta_contraente}{Code referring to the procedure for choosing a contractor}
#'   \item{tipo_scelta_contraente}{Description of the procedure for selecting the contractor}
#'   \item{cod_modalita_realizzazione}{Code referring to the mode of implementation of the tender}
#'   \item{modalita_realizzazione}{Description of the mode of implementation of the tender}
#'   \item{codice_ausa}{AUSA Code (a code assigned by ANAC (italian national anti-corruption authority), which identifies the Contracting Station i.e. authority in the Unique Register of Contracting Stations)}
#'   \item{cf_amministrazione_appaltante}{Fiscal Code of the Administration that tenders or purchases}
#'   \item{denominazione_amministrazione_appaltante}{Name of the administration issuing the invitation to tender or purchase}
#'   \item{sezione_regionale}{Regional reference section for data submission.}
#'   \item{id_centro_costo}{Identifier of the Cost Center, organizational unit of the contracting station.}
#'   \item{denominazione_centro_costo}{Cost Center Name}
#'   \item{anno_pubblicazione}{Reference year, extracted from the date of publication}
#'   \item{mese_pubblicazione}{Reference month, extracted from the date of publication}
#'   \item{cod_cpv}{Commodity category code of the purchased good or service (CPV vocabulary)}
#'   \item{descrizione_cpv}{Description of the commodity category of the good or service purchased}
#'   \item{flag_prevalente}{denotes whether the commodity category is prevailing (a contract may include more than one category of goods or services)}
#'   \item{cup}{Unique Project Code, identifies a public investment project}
#'   \item{id_categoria}{Year}
#'   \item{descrizione}{Year}
#'   \item{descrizione_tipo_categoria}{Year}
#'   \item{modalita_realizzazione_denominazione}{Year}
#'   \item{tipo_scelta_contraente_denominazione}{Year}
#'   \item{data_aggiudicazione_definitiva}{Year}
#'   \item{esito}{Year}
#'   ...
#' }
#' @source <https://dati.anticorruzione.it/opendata>
"mock_data_core"