#' @importFrom SingleCellExperiment reducedDims
#' @import MultiAssayExperiment
NULL

#' Class definition
#'
#' @export
setClass("MultiAssayExtra",
    contains = "MultiAssayExperiment",
    representation(
        reducedDims = "List"
    ),
    prototype(
        reducedDims = List()
    )
)

.getReduced <- function(x) {
    rds <- lapply(experiments(x), function(y) {
        if (hasMethod(reducedDims, signature = class(y)))
            reducedDims(y)
        else
            List()
    })
    rdnames <- unique(unlist(lapply(rds, names)))
    redlist <- List(
        structure(.Data = vector("list", length(rdnames)), .Names = rdnames)
    )
    for (nam in rdnames) {
        res <- lapply(rds, function(y) {
            rg <- y[[nam]]
            if (is.null(rg))
                rg <- matrix()
            else
                rg
            rg
        })
        redlist[[nam]] <- as.array(res)
    }
    redlist
}

#' Constructor
#'
#' @export
MultiAssayExtra <- function(mae, reducedDims) {
    if (missing(reducedDims)) {
        reds <- .getReduced(mae)
    }
    new("MultiAssayExtra",  reducedDims = reds)
}

#' Method def
#'
#' @export
setMethod("reducedDims", "MultiAssayExtra", function(x, ...) {
    x@reducedDims
})
