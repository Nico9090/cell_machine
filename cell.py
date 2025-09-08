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

def plot_sphere(ax, radius, color="blue", alpha=0.3, position=(0,0,0)):
    u = np.linspace(0, 2 * np.pi, 50)
    v = np.linspace(0, np.pi, 50)

    x = radius * np.outer(np.cos(u), np.sin(v)) + position[0]
    y = radius * np.outer(np.sin(u), np.sin(v)) + position[1]
    z = radius * np.outer(np.ones_like(u), np.cos(v)) + position[2]

    ax.plot_surface(x, y, z, color=color, alpha=alpha, linewidth=0)

def plot_ellipsoid(ax, radii=(5,5,5), center=(0,0,0), color="lightblue", alpha=0.3):
    u = np.linspace(0, 2*np.pi, 50)
    v = np.linspace(0, np.pi, 50)

    x = radii[0] * np.outer(np.cos(u), np.sin(v)) + center[0]
    y = radii[1] * np.outer(np.sin(u), np.sin(v)) + center[1]
    z = radii[2] * np.outer(np.ones_like(u), np.cos(v)) + center[2]

    ax.plot_surface(x, y, z, color=color, alpha=alpha, linewidth=0)


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
        fig = plt.figure()
        ax = fig.add_subplot(111,projection="3d")

        plot_sphere(ax, self.cell["outerRadius"],
                    color = "steelblue",
                    alpha = 0.2)
        plot_sphere(ax, self.cell["innerRadius"],
                    color = "red",
                    alpha = 0.3)
        
        for organelle in self.organelles:
            organelle.draw(ax)
        r = self.cell["outerRadius"]
        ax.set_xlim([-r,r])
        ax.set_ylim([-r,r])
        ax.set_zlim([-r,r])
        ax.set_box_aspect([1,1,1])

        plt.show()



