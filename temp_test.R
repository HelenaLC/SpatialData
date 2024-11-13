library(SpatialData)

clean_cache = function(zipname) {
    ca <- BiocFileCache::BiocFileCache()
    ans <- BiocFileCache::bfcquery(ca, zipname)
    if (nrow(ans) > 0) BiocFileCache::bfcremove(ca, ans$rid)
}

# test_that("sandbox data can be acquired and used", {
# clean_cache("mibitof.zip")
tf <- tempfile()
dir.create(tf)
dem <- SpatialData::unzip_spd_demo(
    zipname="mibitof.zip",
    cache=BiocFileCache::BiocFileCache(),
    destination=tf,
    source="sandbox")
grab <- readSpatialData(dem)
x <- grab

library(ggnewscale)
gg8 <- plotSpatialData() + plotImage(grab, 1) + new_scale_fill() + plotLabel(grab, "point8_labels")
ggsave("gg8.png", gg8)
gg16 <- plotSpatialData() + plotImage(grab, 1) + new_scale_fill() + plotLabel(grab, "point16_labels")
ggsave("gg16.png", gg16)
# grab2 <- readSpatialData(dem, anndataR=TRUE)
thing <- 0
#     expect_true(all(dim(SpatialData::table(grab)) == c(36L, 3309L)))
# })

ggcluster <- plotSpatialData() + plotImage(x, 1) + new_scale_fill() + plotLabel(x, c="Cluster")
ggsave("ggcluster.png", ggcluster)



x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x)

ggtest <- plotSpatialData() + plotImage(x, 1) + new_scale_fill() + plotLabel(x, "blobs_labels")
ggsave("ggblobs.png", ggtest)

fun <- c("image", "label", "shape", "point", "table")
