from cell import Cell
from organelle import Organelle
import matplotlib.pyplot as plt
import numpy as np
def showObjectInfo(obj):
    return {
        "objType":type(obj),
        "objAttrMethods":dir(obj),
        "objInst":obj.__dict__,
        "objStruc":vars(obj)
    }

'''    
epithelialCell = Cell()
epithelialCell.makeCell(4,5)
nucleus = Organelle(name = "nucleus",
                    shape = "circle",
                    radius = 0.5)

epithelialCell.addOrganelle(nucleus)

epithelialCellattr = showObjectInfo(epithelialCell)
print(epithelialCellattr["objStruc"])
epithelialCell.drawCell()
'''
epithelialCell = Cell()
epithelialCell.makeCell(4, 8)

nucleus = Organelle(name="nucleus", shape="spherical", radius=3)
epithelialCell.addOrganelle(nucleus)

epithelialCell.drawCell()

# Now you can interact with all geometry data
for g in epithelialCell.geometries:
    print(f"shape={g.x.shape}, center≈({g.x.mean():.2f},{g.y.mean():.2f},{g.z.mean():.2f})")

