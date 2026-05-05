import xarray as xr
import numpy as np
import rioxarray as rxr
from rasterio.transform import from_bounds
import pandas as pd
import glob
import os
import pyreadr
import warnings
warnings.filterwarnings("ignore", category=RuntimeWarning)


#%%%%%%%%%%%%%%%%%%%% obs_bc #%%%%%%%%%%%%%%%%%%%%

path_p = "output/05_prec4pe/prec4pe_obs_bc"

### funcs ###
def get_time(f):
    date_str = f.split("/")[-1].split("_")[-1].replace(".tif", "")
    return pd.to_datetime(date_str)
#############

template_raster = rxr.open_rasterio(os.path.join(path_p, "1960/1960-01-01/grid_1960-01-01.tif")).squeeze()

years = np.arange(1960,2016, 1)
y_files = []

for year in years:

    ## RDS files
    search_pattern_rds = os.path.join(path_p, str(year), "**", "*.RDS")
    rds_files = sorted(glob.glob(search_pattern_rds, recursive=True))

    ## TIF files
    search_pattern_tif = os.path.join(path_p, str(year), "**", "*.tif")
    tif_files = sorted(glob.glob(search_pattern_tif, recursive=True))

    ## time index
    times = [get_time(f) for f in tif_files]

    ## RDS to xr.Dataset
    all_first_rows = []
    for f in rds_files:
        result = pyreadr.read_r(f)
        df = result[None]
        
        first_row = (
            df.iloc[[0]]
            .copy()
            .drop(columns="target", errors="ignore")
            .rename(columns={"date": "analogue"})
        )
        all_first_rows.append(first_row)

    final_df = pd.concat(all_first_rows, ignore_index=True)

    final_df["time"] = times
    final_df = final_df.set_index("time")
    ds_metrics = xr.Dataset.from_dataframe(final_df)

    ## TIF to xr.Dataset
    rasters = [rxr.open_rasterio(f).squeeze() for f in tif_files]

    ## Check CRS consistency
    crs_list = [r.rio.crs for r in rasters]
    if len(set(crs_list)) != 1:
        raise ValueError("All rasters must have the same CRS")

    ## Compute union extent
    bounds = [r.rio.bounds() for r in rasters]

    left   = min(b[0] for b in bounds)
    bottom = min(b[1] for b in bounds)
    right  = max(b[2] for b in bounds)
    top    = max(b[3] for b in bounds)

    if top <= bottom or right <= left:
        raise ValueError("Invalid extent")


    ## Extract original grid (reference)
    template = template_raster
    transform = template.rio.transform()
    res_x = abs(transform.a)
    res_y = abs(transform.e)
    origin_x = transform.c
    origin_y = transform.f

    ## Snap extent to grid (terra-like)
    left_snap = origin_x + np.floor((left - origin_x) / res_x) * res_x
    right_snap = origin_x + np.ceil((right - origin_x) / res_x) * res_x
    top_snap = origin_y + np.ceil((top - origin_y) / res_y) * res_y
    bottom_snap = origin_y + np.floor((bottom - origin_y) / res_y) * res_y

    ## Extend template (preserves coords!)
    template = template.rio.pad_box(
        minx=left_snap,
        miny=bottom_snap,
        maxx=right_snap,
        maxy=top_snap
    )

    # reinforce transform (avoids tiny float drift)
    template = template.rio.write_transform(template.rio.transform())

    ## Align all rasters to template
    rasters_aligned = [
        r.rio.reproject_match(template)
        for r in rasters
    ]

    ## Stack
    stack = xr.concat(rasters_aligned, dim="time")

    ## To Dataset
    ds = stack.to_dataset(dim="band")
    ds = ds.rename({i+1: name for i, name in enumerate(["pr", "err"])})
    ds["time"] = times
    ds = ds.rename({"x": "lon", "y": "lat"})
    ds = ds.drop_vars('spatial_ref')

    encoding = {var: {"zlib": True, "complevel": 5} for var in ds.variables}
    encoding["pr"]["dtype"] = "float32"
    encoding["err"]["dtype"] = "float32"

    ## All
    ds = xr.merge([ds, ds_metrics])

    ## Attrs
    ds["time"].attrs = {
        "standard_name": "time",
        "long_name": "time"
    }
    ds["lon"].attrs = {
        "standard_name": "longitude",
        "long_name": "longitude",
        "units": "degrees_east"
    }

    ds["lat"].attrs = {
        "standard_name": "latitude",
        "long_name": "latitude",
        "units": "degrees_north"
    }

    ds["pr"].attrs = {
        "standard_name": "precipitation_flux",
        "long_name": "precipitation",
        "units": "mm/day"   # adjust if needed!
    }

    ds["err"].attrs = {
        "long_name": "precipitation error",
        "units": "mm/day"
    }

    ds.attrs = {
        "title": "A daily precipitation dataset for Peru using the AC-MSPR framework",
        "institution": "Universität Bern / Geographisches Institut - GIUB",
        "source": "Derived from R terra GeoTIFF",
        "history": "Created " + pd.Timestamp.now().strftime("%Y-%m-%d"),
        "references": "In Progress",
        "Conventions": "CF-1.9"
    }

    ds["dr"].attrs = {
        "long_name": "analogue refined index of agreement",
        "units": "1"
    }

    ds["dr_p90"].attrs = {
        "long_name": "analogue refined index of agreement for the 90th percentile",
        "units": "1"
    }

    ds["mcc"].attrs = {
        "long_name": "analogue matthews correlation coefficient",
        "units": "1"
    }

    ds["mean_metric"].attrs = {
        "long_name": "analogue mean performance metric",
        "units": "1"
    }

    ds["sat"].attrs = {
        "long_name": "analogue satellite product name"
    }

    ds["analogue"].attrs = {
        "long_name": "analogue date"
    }

    print("year:", year, "n_anlgs:", len(rds_files), "n_days", len(tif_files))
    y_files.append(ds)

ds_all = xr.concat(y_files, dim="time")
ds_all.to_netcdf("/scratch2/ahuerta/ac-mspr/prec4pe_obs_bc.nc",
                 encoding=encoding,
                 engine="netcdf4")

# to zarr

zds = xr.open_dataset('/scratch2/ahuerta/ac-mspr/prec4pe_obs_bc.nc', chunks={'time': 365})
zds_zo = ds.chunk({'time': 365, 'lat': -1, 'lon': -1})
for var in zds_zo.data_vars:
    if 'chunks' in zds_zo[var].encoding:
        del zds_zo[var].encoding['chunks']
zds_zo.to_zarr('/scratch2/ahuerta/ac-mspr/prec4pe_obs_bc.zarr', mode='w', consolidated=True)


#%%%%%%%%%%%%%%%%%%%% hmg_obs_bc #%%%%%%%%%%%%%%%%%%%%

path_p = "output/05_prec4pe/prec4pe_hmg_obs_bc"

### funcs ###
def get_time(f):
    date_str = f.split("/")[-1].split("_")[-1].replace(".tif", "")
    return pd.to_datetime(date_str)
#############

template_raster = rxr.open_rasterio(os.path.join(path_p, "1960/1960-01-01/grid_1960-01-01.tif")).squeeze()

years = np.arange(1960,2016, 1)
y_files = []

for year in years:

    ## RDS files
    search_pattern_rds = os.path.join(path_p, str(year), "**", "*.RDS")
    rds_files = sorted(glob.glob(search_pattern_rds, recursive=True))

    ## TIF files
    search_pattern_tif = os.path.join(path_p, str(year), "**", "*.tif")
    tif_files = sorted(glob.glob(search_pattern_tif, recursive=True))

    ## time index
    times = [get_time(f) for f in tif_files]

    ## RDS to xr.Dataset
    all_first_rows = []
    for f in rds_files:
        result = pyreadr.read_r(f)
        df = result[None]
        
        first_row = (
            df.iloc[[0]]
            .copy()
            .drop(columns="target", errors="ignore")
            .rename(columns={"date": "analogue"})
        )
        all_first_rows.append(first_row)

    final_df = pd.concat(all_first_rows, ignore_index=True)

    final_df["time"] = times
    final_df = final_df.set_index("time")
    ds_metrics = xr.Dataset.from_dataframe(final_df)

    ## TIF to xr.Dataset
    rasters = [rxr.open_rasterio(f).squeeze() for f in tif_files]

    ## Check CRS consistency
    crs_list = [r.rio.crs for r in rasters]
    if len(set(crs_list)) != 1:
        raise ValueError("All rasters must have the same CRS")

    ## Compute union extent
    bounds = [r.rio.bounds() for r in rasters]

    left   = min(b[0] for b in bounds)
    bottom = min(b[1] for b in bounds)
    right  = max(b[2] for b in bounds)
    top    = max(b[3] for b in bounds)

    if top <= bottom or right <= left:
        raise ValueError("Invalid extent")


    ## Extract original grid (reference)
    template = template_raster
    transform = template.rio.transform()
    res_x = abs(transform.a)
    res_y = abs(transform.e)
    origin_x = transform.c
    origin_y = transform.f

    ## Snap extent to grid (terra-like)
    left_snap = origin_x + np.floor((left - origin_x) / res_x) * res_x
    right_snap = origin_x + np.ceil((right - origin_x) / res_x) * res_x
    top_snap = origin_y + np.ceil((top - origin_y) / res_y) * res_y
    bottom_snap = origin_y + np.floor((bottom - origin_y) / res_y) * res_y

    ## Extend template (preserves coords!)
    template = template.rio.pad_box(
        minx=left_snap,
        miny=bottom_snap,
        maxx=right_snap,
        maxy=top_snap
    )

    # reinforce transform (avoids tiny float drift)
    template = template.rio.write_transform(template.rio.transform())

    ## Align all rasters to template
    rasters_aligned = [
        r.rio.reproject_match(template)
        for r in rasters
    ]

    ## Stack
    stack = xr.concat(rasters_aligned, dim="time")

    ## To Dataset
    ds = stack.to_dataset(dim="band")
    ds = ds.rename({i+1: name for i, name in enumerate(["pr", "err"])})
    ds["time"] = times
    ds = ds.rename({"x": "lon", "y": "lat"})
    ds = ds.drop_vars('spatial_ref')

    encoding = {var: {"zlib": True, "complevel": 5} for var in ds.variables}
    encoding["pr"]["dtype"] = "float32"
    encoding["err"]["dtype"] = "float32"

    ## All
    ds = xr.merge([ds, ds_metrics])

    ## Attrs
    ds["time"].attrs = {
        "standard_name": "time",
        "long_name": "time"
    }
    ds["lon"].attrs = {
        "standard_name": "longitude",
        "long_name": "longitude",
        "units": "degrees_east"
    }

    ds["lat"].attrs = {
        "standard_name": "latitude",
        "long_name": "latitude",
        "units": "degrees_north"
    }

    ds["pr"].attrs = {
        "standard_name": "precipitation_flux",
        "long_name": "precipitation",
        "units": "mm/day"   # adjust if needed!
    }

    ds["err"].attrs = {
        "long_name": "precipitation error",
        "units": "mm/day"
    }

    ds.attrs = {
        "title": "A daily precipitation dataset for Peru using the AC-MSPR framework",
        "institution": "Universität Bern / Geographisches Institut - GIUB",
        "source": "Derived from R terra GeoTIFF",
        "history": "Created " + pd.Timestamp.now().strftime("%Y-%m-%d"),
        "references": "In Progress",
        "Conventions": "CF-1.9"
    }

    ds["dr"].attrs = {
        "long_name": "analogue refined index of agreement",
        "units": "1"
    }

    ds["dr_p90"].attrs = {
        "long_name": "analogue refined index of agreement for the 90th percentile",
        "units": "1"
    }

    ds["mcc"].attrs = {
        "long_name": "analogue matthews correlation coefficient",
        "units": "1"
    }

    ds["mean_metric"].attrs = {
        "long_name": "analogue mean performance metric",
        "units": "1"
    }

    ds["sat"].attrs = {
        "long_name": "analogue satellite product name"
    }

    ds["analogue"].attrs = {
        "long_name": "analogue date"
    }

    print("year:", year, "n_anlgs:", len(rds_files), "n_days", len(tif_files))
    y_files.append(ds)

ds_all = xr.concat(y_files, dim="time")
ds_all.to_netcdf("/scratch2/ahuerta/ac-mspr/prec4pe_hmg_obs_bc.nc",
                 encoding=encoding,
                 engine="netcdf4")

# to zarr

zds = xr.open_dataset('/scratch2/ahuerta/ac-mspr/prec4pe_hmg_obs_bc.nc', chunks={'time': 365})
zds_zo = ds.chunk({'time': 365, 'lat': -1, 'lon': -1})
for var in zds_zo.data_vars:
    if 'chunks' in zds_zo[var].encoding:
        del zds_zo[var].encoding['chunks']
zds_zo.to_zarr('/scratch2/ahuerta/ac-mspr/prec4pe_hmg_obs_bc.zarr', mode='w', consolidated=True)

