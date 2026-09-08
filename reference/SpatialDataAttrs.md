# The \`SpatialDataAttrs\` class

The \`SpatialDataAttrs\` class

## Usage

``` r
SpatialDataAttrs(
  x,
  type = c("image", "label", "point", "shape"),
  trans = NULL,
  ver = NULL,
  dim = 2,
  nch = 3
)

# S4 method for class 'SpatialDataAttrs'
x$name

# S4 method for class 'SpatialDataPoint'
feature_key(x)

# S4 method for class 'SpatialDataAttrs'
feature_key(x)

# S4 method for class 'SpatialDataAttrs,character'
feature_key(x) <- value

# S4 method for class 'SingleCellExperiment'
region_key(x)

# S4 method for class 'SingleCellExperiment'
region(x)

# S4 method for class 'SingleCellExperiment'
regions(x)

# S4 method for class 'SingleCellExperiment,character'
regions(x) <- value

# S4 method for class 'SingleCellExperiment,NULL'
regions(x) <- value

# S4 method for class 'list'
instance_key(x)

# S4 method for class 'SingleCellExperiment'
instance_key(x)

# S4 method for class 'SpatialDataFrame'
instance_key(x)

# S4 method for class 'SpatialDataLabel'
instance_key(x)

# S4 method for class 'SpatialDataAttrs,character'
instance_key(x) <- value

# S4 method for class 'SingleCellExperiment,character'
instance_key(x) <- value

# S4 method for class 'SpatialDataLabel'
instances(x)

# S4 method for class 'SpatialDataPoint'
instances(x)

# S4 method for class 'SpatialDataShape'
instances(x)

# S4 method for class 'SingleCellExperiment'
instances(x)

# S4 method for class 'SingleCellExperiment'
instances(x) <- value
```

## Arguments

- x:

  element or list extracted from a OME-NGFF compliant .zattrs file.

- type:

  character string; either "image", "label", "point" or "shape"

- trans:

  list of coordinate transformations; defaults to identity only.

- ver:

  character string; specifies the version of the SpatialData element to
  comply with.

- dim:

  scalar integer in 2-4 when `type` is either "image" or "label", and
  2-3 when "shape" or "point"; number of dimensions: 2 = XY, 3 adds Z, 4
  adds T (time) for image and label; when `type="image"`, C (channel)
  will be added (for any `dim`).

- nch:

  scalar integer; how many channels should there be? (ignored if
  `type="label"`, `type="shape"`, or `type="point"`).

- name:

  character string for extraction (see ?base::\`\$\`).

- value:

  character string (for one `region` and `_key`s), or vector (for many
  `region`s, `instances` and `regions`).

## Value

SpatialDataAttrs object

## Details

When `x` is a spatial element, the following applies:
`SpatialDataFrame`: `feature/instance_key`, `SingleCellExperiment`:
`region`, `region/instance_key`.

When missing `x`, `SpatialDataAttrs` will generate a valid object with
default axes (image: cyx, label: yx, point/shape: xy) and
transformations (identify) according to the specified type.

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x)

# tables
region(table(x))
#> [1] "blobs_labels"
region_key(table(x))
#> [1] "region"

# points
instance_key(point(x))
#> [1] "instance_id"
fk <- feature_key(point(x))
base::table(point(x)[[fk]])
#> 
#> gene_a gene_b 
#>     90    110 

# transformations
(z <- meta(label(x)))
#> class: SpatialDataAttrs
#> axes(2):
#> - name: y x 
#> - type: space space 
#> coordTrans(5):
#> - global: (identity)
#> - scale: (scale:[3,2])
#> - translation: (translation:[-50,10])
#> - affine: (affine:[[20,10,30],[50,40,60]])
#> - sequence: (scale:[3,2]), (translation:[-50,10])
#> datasets(1): 0
#> - 0: (scale:[1,1])
CTname(z)
#> [1] "global"      "scale"       "translation" "affine"      "sequence"   
CTtype(z)
#> [1] "identity"    "scale"       "translation" "affine"      "sequence"   
CTdata(z, "scale")
#> [[1]]
#> [1] 3
#> 
#> [[2]]
#> [1] 2
#> 

# constructor
SpatialDataAttrs(type="point")
#> class: SpatialDataAttrs
#> axes(2):
#> - name: x y 
#> coordTrans(1):
#> - global: (identity)
SpatialDataAttrs(type="shape")
#> class: SpatialDataAttrs
#> axes(2):
#> - name: x y 
#> coordTrans(1):
#> - global: (identity)
SpatialDataAttrs(type="image", nch=7)
#> class: SpatialDataAttrs
#> axes(3):
#> - name: c y x 
#> - type: channel space space 
#> coordTrans(1):
#> - global: (identity)
#> datasets(1): 0
#> - 0: (scale:[1,1,1])
#> channels(7): a b ... f g
SpatialDataAttrs(type="label", dim=3)
#> class: SpatialDataAttrs
#> axes(3):
#> - name: z y x 
#> - type: space space space 
#> coordTrans(1):
#> - global: (identity)
#> datasets(1): 0
#> - 0: (scale:[1,1,1])
```
