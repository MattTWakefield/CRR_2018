import os
import arcpy as py

workspace = r"C:\GIS\CRR\CRR_2018\Data"
outWorkspace = r"C:\GIS\CRR\CRR_2018\Data"
py.env.overwriteOutput = True

targetFeatures = os.path.join(workspace, r"blocksf\tabblock2010_47_pophu.shp")
joinFeatures = os.path.join(workspace, r"FD Boundaries.gdb\FD_BoundariesDec2017_V2")
join_type = "KEEP_ALL"
match_option = "HAVE_THEIR_CENTER_IN"
join_operation = 'JOIN_ONE_TO_ONE'

outfc = os.path.join(outWorkspace, r"FD_Blocks.gdb\FD_Blocks")

py.SpatialJoin_analysis(targetFeatures, 
                      joinFeatures, 
                      outfc, 
                      join_operation, 
                      join_type, 
                      match_option = match_option)