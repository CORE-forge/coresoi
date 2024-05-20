#' Italian _Banca Dati Nazionali dei Contratti Pubblici_ (BDNCP)
#'
#' Data on public contracts in Italy are published in open format on the [portal](https://dati.anticorruzione.it/opendata/dataset),
#' handled by the Italian anticorruption authority (ANAC).
#'
#' @format ## `mock_data_core`
#' A data frame with approximately 300,000 contracts and 39 variables:
#' \describe{
#'   \item{cig}{Contract identification code: it is a code assigned by the Authority to uniquely track tenders and contracts nationwide
#'   and is assigned at the lot level.}
#'   \item{importo_lotto}{Economic amount (starting bid).}
#'   \item{oggetto_lotto}{Object (string).}
#'   \item{oggetto_principale_contratto}{Main contract object: works ("LAVORI") , services ("SERVIZI"), supplies ("FORNITURE").}
#'   \item{cod_cpv}{Common Procurement Vocabulary (CPV) code.}
#'   \item{descrizione_cpv}{Common Procurement Vocabulary (CPV) description.}
#'   \item{flag_prevalente}{Dummy, indicating whether the CPV is prevailing }
#'   \item{luogo_istat}{Contract geographic localisation (ISTAT code).}
#'   \item{data_pubblicazione}{Publication date of the call for tenders.}
#'   \item{cod_tipo_scelta_contraente}{Code referring to the procedure type for choosing a contractor.}
#'   \item{tipo_scelta_contraente}{Description of the procedure type for selecting the contractor.}
#'   \item{cod_modalita_realizzazione}{Code referring to the mode of implementation of the contract.}
#'   \item{modalita_realizzazione}{Description of the mode of implementation of the contract.}
#'   \item{cf_amministrazione_appaltante}{ID code (_codice fiscale_) of the contracting authorities issuing the contract.}
#'   \item{denominazione_amministrazione_appaltante}{Name of the contracting authorities issuing the contract.}
#'   \item{citta_codice_stazapp}{Contracting authority geographic location (ISTAT code).}
#'   \item{id_aggiudicazione}{Award notice ID.}
#'   \item{data_aggiudicazione_definitiva}{Award notice date.}
#'   \item{importo_aggiudicazione}{Award value.}
#'   \item{criterio_aggiudicazione}{Award criterion (most economically advantageous bid, lowest price, etc.).}
#'   \item{numero_offerte_ammesse}{Number of eligible bids.}
#'   \item{num_imprese_offerenti}{Number of received bids.}
#'   \item{data_termine_contrattuale}{Expected end date.}
#'   \item{data_inizio_effettiva}{Actual start date.}
#'   \item{data_effettiva_ultimazione}{Actual end date.}
#'   \item{aggiudicatari}{*Nested* dataframe containing basic information on the awarded company(ies):
#'   `codice_fiscale` (Company ID) and `denominazione` (company denomination).}
#'   \item{varianti}{*Nested* dataframe containing information on the modification(s) occured:
#'   `id_variante`(variant ID), `data_approvazione_variante` (variant date), `cod_motivo_variante` (variant reason code),
#'   and `motivo_variante` (variant reason description).}
#'   \item{cod_regione}{Region ISTAT code (NUTS-2 level).}
#'   \item{nome_regione}{Region name (NUTS-2 level).}
#'   \item{cod_provincia}{Province ISTAT code (NUTS-3 level).}
#'   \item{nome_provincia}{Province name (NUTS-3 level).}
#'   \item{cod_comune}{Municipality ISTAT code (local administrative units).}
#'   \item{nome_comune}{Municipality name (local administrative units).}
#'   \item{rip_geo}{Geographic macro-level (NUTS-1 level).}
#'   \item{codice_nuts1_2021}{NUTS-1 code.}
#'   \item{codice_nuts2_2021}{NUTS-2 code.}
#'   \item{codice_nuts3_2021}{NUTS-3 code.}
#' }
#' @source <https://dati.anticorruzione.it/opendata>
#' @details Nested dataframes (see `aggiudicatari` and `varianti`) can be unnested using function [tidyr::unnest()].
"mock_data_core"
