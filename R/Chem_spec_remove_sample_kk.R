remove_sample<-function (spectra, kill, group = TRUE)
{
    .chkArgs(mode = 0L)
    if (is.character(kill)) {
        drop <- NA_integer_
        for (n in 1:length(kill)) {
            if (group)
                more <- grep(kill[n], spectra$groups)
            if (!group)
                more <- grep(kill[n], spectra$names)
            drop <- c(drop, more)
        }
        kill <- drop[-1]
    }
    if ((length(kill) == 0L) & (group))
        stop("No matching groups found to remove")
    if ((length(kill) == 0L) & (!group))
        stop("No matching samples found to remove")
    if (inherits(spectra, "Spectra")) {
        spectra$data <- spectra$data[-kill, ]
        spectra$sym <- spectra$sym[-kill]
        spectra$alt.sym <- spectra$alt.sym[-kill]
    }
    if (inherits(spectra, "Spectra2D"))
        spectra$data <- spectra$data[-kill, drop = FALSE]
    spectra$names <- spectra$names[-kill]
    spectra$groups <- spectra$groups[-kill, drop = TRUE]
    spectra$colors <- spectra$colors[-kill]
    spectra$cal_dat <- spectra$cal_dat[-kill,]
    spectra$all_dat <- spectra$all_dat[-kill,]
    if (length(spectra$names) == 0)
        warning("You have removed all your samples!")
    jnk <- .extraData(spectra)
    chkSpectra(spectra)
    return(spectra)
}
