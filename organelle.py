from dataclasses import dataclass
from typing import Optional
import matplotlib.patches as patches
import matplotlib.pyplot as plt
@dataclass
class Organelle():
    name: str
    shape: str
    size: Optional[float]=None
    radius: Optional[float]=None

    def draw(self, ax, position=(0,0)):
        if self.shape == "circle" and self.radius:
            obj = plt.Circle(position,
                             self.radius,
                             color = "white")
            ax.add_artist(obj)

def draw(self, ax, position=(0,0,0)):
        if self.shape == "circle" and self.radius:
            u = np.linspace(0, 2*np.pi, 20)
            v = np.linspace(0,np.pi,20)
            x = self.radius * np.outer(np.cos(u), np.sin(v)) + position[0]
            y = self.radius * np.outer(np.sin(u), np.sin(v)) + position[1]
            z = self.radius * np.outer(np.ones_like(u), np.cos(v)) + position[2]
            ax.plot_surface(x,y,z, color="white" if self.name=="nucleus" else "lightblue", alpha = 0.6)


    
