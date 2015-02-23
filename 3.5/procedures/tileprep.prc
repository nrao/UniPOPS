procedure tileprep(delx, nx, dely, ny, xcpos, ycpos)
#   delx and dely are in arcseconds, they are the coord. increment between tiles
#   nx and ny are the total number of tiles in each direction
#   xcpos and ycpos are the center positions (in decimal degrees).
#   Leaves 30 pixels unused in Y direction (line for TITLE)
global scalar tile_xc, tile_yc, tile_xpixc, tile_ypixc, tile_delx, tile_dely
global scalar tile_xsize, tile_ysize
scalar txpix, typix, ddx, ddy
#
txpix = status(23); typix = status(24) - 30
tile_xc = xcpos; tile_yc = ycpos
tile_xpixc = nint(txpix / 2); tile_ypixc = nint(typix / 2) + 30
ddx = delx / 3600.0; ddy = dely/3600.0
tile_delx = txpix / (nx * ddx); tile_dely = typix / (ny * ddy)
tile_xsize = ifix(txpix/nx); tile_ysize = ifix(typix/ny)
#
return
finish
