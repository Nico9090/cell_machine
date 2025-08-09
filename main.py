from cell import Cell
from grow import Grow
from organelles import AddOrganelle

epithelial=Grow(2,5,3)
print(epithelial.append_organelle_list("nucleus"))
print("Cell contents: ",epithelial.organelle_details())
#print("Surface area: ", epithelial.calculate_cell_surface_area())
#print("Volume: ", epithelial.calculate_volume())

