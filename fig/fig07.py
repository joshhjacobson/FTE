import numpy as np
from netCDF4 import Dataset

import matplotlib.pyplot as plt
import matplotlib.patches as patches
import matplotlib.path as path
from matplotlib import cm
from mpl_toolkits.axes_grid1 import ImageGrid, make_axes_locatable
from matplotlib.colors import ListedColormap


## Establish event date
iday = 24
imonth = 7
iyear = 2004
imem = 1


##  Load GEFS and downscaled data
infile = "../data/refcstv2_precip_ccpav3_subset_066_to_072.nc"
nc = Dataset(infile)
yyyymmddhh_init = nc.variables["yyyymmddhh_init"][:]
lons_anal = nc.variables["lons_anal"][:, :]
lats_anal = nc.variables["lats_anal"][:, :]
lons_fcst = nc.variables["lons_fcst"][4:-4, 4:-4]
lats_fcst = nc.variables["lats_fcst"][4:-4, 4:-4]
apcp_fcst_ens = nc.variables["apcp_fcst_ens"][:, :, 4:-4, 4:-4]
apcp_anal = nc.variables["apcp_anal"][:, :, :]
apcp_anal_upsc = nc.variables["apcp_anal_upsc"][:, 4:-4, 4:-4]
nc.close()

infile = f"../data/GSDM_downscaled_0{imonth}_066_to_072.nc"
nc = Dataset(infile)
fcst_dwnsc = nc.variables["downscaled"][iyear - 2002, iday - 1, :, :, :]
nc.close()

ndays, nens, nyf, nxf = apcp_fcst_ens.shape
ndays, nya, nxa = apcp_anal.shape

yyyymmddhh_verif_init = int(iyear * 1e6 + imonth * 1e4 + iday * 1e2)
iday_gefs = np.where(yyyymmddhh_init == yyyymmddhh_verif_init)[0][0]


## Set up colormap and coordinates for plotting
BuPu = cm.get_cmap("BuPu", 12)
TF = ListedColormap(["#f7fcfd", "#08306b"])
states_us = np.load("../data/states_us.npz", allow_pickle=True,)["polygons"].tolist()

dxa = lons_anal[0, 1] - lons_anal[0, 0]
dya = lats_anal[1, 0] - lats_anal[0, 0]
extenta = [
    lons_anal[0, 0] - dxa / 2.0,
    lons_anal[0, -1] + dxa / 2.0,
    lats_anal[0, 0] - dya / 2.0,
    lats_anal[-1, 0] + dya / 2.0,
]

dxf = lons_fcst[0, 1] - lons_fcst[0, 0]
dyf = lats_fcst[1, 0] - lats_fcst[0, 0]
extentf = [
    lons_fcst[0, 0] - dxf / 2.0,
    lons_fcst[0, -1] + dxf / 2.0,
    lats_fcst[0, 0] - dyf / 2.0,
    lats_fcst[-1, 0] + dxf / 2.0,
]


## Set up figure and image grid
fig = plt.figure(figsize=(12, 9))

grid = ImageGrid(
    fig,
    111,
    nrows_ncols=(2, 3),
    axes_pad=0.15,
    share_all=True,
    cbar_location="right",
    cbar_mode="edge",
    direction="row",
    cbar_pad=0.15,
)

## Add data to image grid
images = []

grid[0].set_title("GEFS member", fontsize=13, fontweight="bold")
images.append(
    grid[0].imshow(
        apcp_fcst_ens[iday_gefs, imem, ::-1, :], cmap=BuPu, extent=extentf, vmax=52.0
    )
)
images.append(
    grid[3].imshow(apcp_fcst_ens[iday_gefs, imem, ::-1, :] > 5, cmap=TF, extent=extentf)
)


grid[1].set_title("Downscaled member", fontsize=13, fontweight="bold")
images.append(
    grid[1].imshow(fcst_dwnsc[imem, ::-1, :], cmap=BuPu, extent=extentf, vmax=52.0)
)
images.append(grid[4].imshow(fcst_dwnsc[imem, ::-1, :] > 5, cmap=TF, extent=extentf))


grid[2].set_title("CCPA Analysis", fontsize=13, fontweight="bold")
images.append(
    grid[2].imshow(apcp_anal[iday_gefs, ::-1, :], cmap=BuPu, extent=extenta, vmax=52.0)
)
images.append(
    grid[5].imshow(apcp_anal[iday_gefs, ::-1, :] > 5, cmap=TF, extent=extentf)
)


## Add state lines to images
for k in range(len(states_us)):
    pathPolygon = path.Path(states_us[str(k)])
    for i in range(len(grid)):
        grid[i].add_patch(patches.PathPatch(pathPolygon, facecolor="none", lw=0.5))


## Remove ticks
grid[0].get_yaxis().set_ticks([])
grid[0].get_xaxis().set_ticks([])

## Set top colorbar, remove lower colorbar
grid[2].cax.colorbar(images[2])
grid[2].cax.set_ylabel(
    "Accumulated precipitation (mm)", fontsize=11, rotation=270, labelpad=15
)
grid[2].cax.toggle_label(True)
cb = grid[5].cax
cb.remove()

fig.savefig("fig07.png", dpi=300, bbox_inches="tight")
