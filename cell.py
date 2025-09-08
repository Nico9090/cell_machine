import matplotlib.pyplot as plt
import numpy as np
from mpl_toolkits.mplot3d import Axes3D  # needed for 3D projection
from organelle import Organelle
class Cell():
    def __init__(self):
        self.cell = None
        self.organelles:List[Organelle]=[]
    def makeCell(self,innerRadius,outerRadius):
        if outerRadius > innerRadius:
            self.cell = {
                "innerRadius":innerRadius,
                "outerRadius":outerRadius,
                "outerArea":np.pi*outerRadius**2,
                "innerArea":np.pi*innerRadius**2,
                "membraneArea": (np.pi*outerRadius**2) - (np.pi*innerRadius**2)
                }
        else:
            return f"Error: outerRadius must be greater than innerRadius"
        return self.cell
    def addOrganelle(self,organelle):
        if isinstance(organelle,Organelle):
            self.organelles.append(organelle)
            print(f"Added {organelle} to cell")
        else:
            print("Not a valid organelle")
    def drawCell(self):
        if not self.cell:
            return f"Error: cell not created"
        fig, ax = plt.subplots()
        outerMembrane = plt.Circle((0,0), self.cell["outerRadius"],
                                   color="steelblue")
        innerMembrane = plt.Circle((0,0), self.cell["innerRadius"],
                                   color="red")
        ax.add_artist(outerMembrane)
        ax.add_artist(innerMembrane)
        r = self.cell["outerRadius"]
        ax.set_xlim([-r-1,r+1])
        ax.set_ylim([-r-1,r+1])
        ax.set_aspect('equal')

        for i,organelle in enumerate(self.organelles):
            organelle.draw(ax)
        plt.show()




