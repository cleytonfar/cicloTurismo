#' Calculate intersection between two cycling networks
#' 
#' @description 
#' This function will calculate the intersection between two cycling networks.
#' 
#' @details 
#' The calculation is done by using a buffering of one of the cycling network (5 meters),
#' and then calculate the intersection with he other cycling network. #' 
#' 
#' @param malhaPermanente data.frame/data.table object with ride information from Strava. 
#' This is the result from the processing_strava() function.
#' @param malhaOperacional data.frame with the geographical features from the 
#' cycling network. Basically, it is the object resulted from reading the .shp file
#' using the sf package.
#'
#' @return a data.table object with information from malhaPermanente with the
#' calculated intersection with malhaOperacional.
#' 
#' @importFrom sf st_intersection st_buffer st_length
#' 
#' @export

calcIntersectionMalhas <- function(malhaPermanente, 
                                   malhaOperacional
                                   ) {
    # calculando a interseção:
    res = st_intersection(
        malhaPermanente,
        st_buffer(malhaOperacional, dist = 5)
    )
    
    # calculando a extensão
    res = res %>% dplyr::mutate(length = as.numeric(st_length(geometry)))
    
    # é considerado interseção se duas malhas (permanente e operacional) possuem
    # o mesmo sentido
    res = res %>% dplyr::filter(Sentido == Sentido.1)
    res
}
