import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import rebound
import pandas as pn
import os

directory = "/media/rainbow/Swap/Swap/Thesis/Notable/Ephemerides/"

orbits = []
for file in os.listdir(directory):
    if file.endswith(".ephem"):
        loading = pn.read_csv(directory+file, delimiter=r"\s+", skiprows=1, names=["JD","x","y","z"], header=None)
    orbits.append(loading)

# Set up the figure and subplots
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(8, 8))
ax1.set_aspect('equal')
ax1.set_xlim(-8, 8)
ax1.set_ylim(-8, 8)

ax2.set_aspect('auto')
ax2.set_xlim(-8, 8)
ax2.set_ylim(-4, 4)

for asteroid in orbits:
            ax1.plot(asteroid.x, asteroid.y, 'lightgray', linewidth=1, zorder=1, alpha=0.5)

for asteroid in orbits:
            ax2.plot(asteroid.x, asteroid.z, 'lightgray', linewidth=1, zorder=1, alpha=0.5)

scat = ax1.scatter([orbit.x for orbit in orbits], 
                   [orbit.y for orbit in orbits],
                   c='red', s=1, zorder=2)
scat_xz = ax2.scatter([orbit.x for orbit in orbits], 
                      [orbit.z for orbit in orbits],
                      c='red', s=1, zorder=2)

def update(frame):
    # x_vals = orbits.x[frame % len(orbits.x)]
    # y_vals = orbits.y[frame % len(orbits.y)]
    # z_vals = orbits.z[frame % len(orbits.z)]

    x_vals = [orbit.x[frame % len(orbits[1].x)] for orbit in orbits]
    y_vals = [orbit.y[frame % len(orbits[1].y)] for orbit in orbits]
    z_vals = [orbit.z[frame % len(orbits[1].z)] for orbit in orbits]

    data_xy = np.column_stack((x_vals, y_vals))
    scat.set_offsets(data_xy)

    data_xz = np.column_stack((x_vals, z_vals))
    scat_xz.set_offsets(data_xz)

    return scat, scat_xz

ani = animation.FuncAnimation(fig=fig, func=update, frames=len(orbits[1].JD)//2, interval=5)
plt.tight_layout()
plt.show()
ani.save("test_animation.gif", writer='pillow', fps=30)