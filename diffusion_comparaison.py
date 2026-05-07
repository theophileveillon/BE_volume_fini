import numpy as np
import matplotlib.pyplot as plt
import sys
import math

if len(sys.argv) != 2:
    print("Usage: python diffusion_comparaison.py <time_step_index>")
    sys.exit(1)

# ===== Lecture du fichier =====
filename = "x_L_2.dat"
t = int(sys.argv[1])

with open(filename, "r") as f:
    lines = f.readlines()

# première ligne : paramètres
header = lines[0].split()
nx, ny = int(header[0]), int(header[1])
xcst, L = float(header[2]), float(header[3])
dt, nT = float(header[4]), int(header[5])
k = float(header[6])

# ===== Lecture des données =====
data = []
for line in lines[1:]:
    values = np.array([float(v) for v in line.strip().split(";")])
    data.append(values)

data = np.array(data)   # shape = (temps, ny)

# ===== Axe y =====
y = np.linspace(0, L, ny)
print(f"kappa = {k}")
# ===== Concentration théorique =====
c_th = np.zeros(ny)
for j in range(ny):
    c_th[j] = 1/2 * (1 - math.erf((j*L/ny-L/2) / (2*math.sqrt(k*t))))

# ===== Plot =====
plt.figure(figsize=(8,5))

plt.plot(y, data[int(t/dt)], label=f"C")
plt.plot(y, c_th, label=f"c_th")

# ===== Zoom automatique (important pour petites valeurs) =====
ymin = np.min(data[int(t/dt)])
ymax = np.max(data[int(t/dt)])

plt.xlabel("y")
plt.ylabel("c(y, x = L)")
plt.title("Evolution du profil de concentration en x=L a t = {:.0f} s".format(t))
plt.legend()
plt.grid()

plt.savefig("diffusion_comparaison.png", dpi=300)
plt.show()