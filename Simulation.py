import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import matplotlib.cm as cm
import matplotlib
import rebound
import pandas as pn
import os
import distinctipy

class Orbit:
    def __init__(self, id, JD, x, y, z, color):
        self.id = id
        self.JD = JD
        self.x = x
        self.y = y
        self.z = z
        self.color = color

    def __repr__(self):
        return f"ID={self.id}, Date={self.JD}, Color={self.color})"

directory = "/media/rainbow/Swap/Swap/Thesis/Notable/Ephemerides/"

orbits = []
for file in os.listdir(directory):
    orbit_data = pn.read_csv(directory+file, delimiter=r"\s+", skiprows=1, names=["JD","x","y","z"], header=None)
    orbit = Orbit(file, orbit_data.JD, orbit_data.x, orbit_data.y, orbit_data.z, "lightgray")
    orbits.append(orbit)

# Set up the figure and subplots
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(10, 10))
fig.suptitle("Calculated Orbits of Linked Asteroid Detections")
ax1.set_aspect('equal')
ax1.set_xlabel("x")
ax1.set_ylabel("y")
ax1.set_xlim(-8, 8)
ax1.set_ylim(-8, 8)

ax2.set_aspect('equal')
ax2.set_xlabel("x")
ax2.set_ylabel("z")
ax2.set_xlim(-8, 8)
ax2.set_ylim(-8, 8)

angle = np.linspace( 0,2 * np.pi, 150 ) 
radius = 1 
earthx = radius * np.cos(angle) 
earthy = radius * np.sin(angle)
earthz = 0 * np.sin(angle)  
ax1.plot(earthx, earthy, 'skyblue', linewidth=2, zorder=1, alpha=0.9)
ax2.plot(earthx, earthz, 'skyblue', linewidth=2, zorder=1)

for asteroid in orbits:
            ax1.plot(asteroid.x, asteroid.y, c='lightgray', linewidth=1, zorder=1, alpha=0.5)

for asteroid in orbits:
            ax2.plot(asteroid.x, asteroid.z, c='lightgray', linewidth=1, zorder=1, alpha=0.5)

n_objects = len(orbits)
# cmap = matplotlib.colormaps.get_cmap('turbo')  # or try 'gist_ncar', 'nipy_spectral', 'turbo'
# object_colors = {i: cmap(i / n_objects) for i in range(n_objects)}  # normalized            
object_colors = distinctipy.get_colors(n_objects, pastel_factor=0.7)

for i, orbit in enumerate(orbits):
    orbit.color = object_colors[i]


# Later when plotting
scat = ax1.scatter([orbit.x[1] for orbit in orbits],
                   [orbit.y[1] for orbit in orbits],
                   s = 5, zorder = 2, color=[orbit.color for orbit in orbits], marker="*")
scat_xz = ax2.scatter([orbit.x[1] for orbit in orbits],
                      [orbit.z[1] for orbit in orbits],
                      s=5,zorder=2, color=[orbit.color for orbit in orbits], marker="*")
# scat = ax1.scatter([orbit.x for orbit in orbits], 
#                    [orbit.y for orbit in orbits],   
#                    s=1, zorder=2, color=colors_for_plot)
# scat_xz = ax2.scatter([orbit.x for orbit in orbits], 
#                       [orbit.z for orbit in orbits],  
#                       s=1, zorder=2, color=colors_for_plot)

def update(frame):
    x_vals = [orbit.x[frame % len(orbits[1].x)] for orbit in orbits]
    y_vals = [orbit.y[frame % len(orbits[1].y)] for orbit in orbits]
    z_vals = [orbit.z[frame % len(orbits[1].z)] for orbit in orbits]

    data_xy = np.column_stack((x_vals, y_vals))
    scat.set_offsets(data_xy)

    data_xz = np.column_stack((x_vals, z_vals))
    scat_xz.set_offsets(data_xz)    

    return scat, scat_xz


ani = animation.FuncAnimation(fig=fig, func=update, frames=len(orbits[1].JD), interval=20, blit=True)
plt.tight_layout()
#plt.show()
writer = animation.FFMpegWriter(
     fps=24, metadata=dict(artist='Me'), bitrate=1800)
ani.save("orbits.mp4", writer=writer)
# ani.save("test_animation.mp4", writer='pillow', fps=30)