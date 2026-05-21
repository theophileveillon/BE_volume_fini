import numpy as np
import matplotlib.pyplot as plt
import sys
import math

if len(sys.argv) < 2:
    print("Usage: python plot_courbe.py mode")
    sys.exit(1)

#====== Lecture du mode ======
mode = sys.argv[1]

def read_multi_constante_file(filename):
    with open(filename, "r") as f:
        lines = f.readlines()

    # ===== Header global (ligne 0) =====
    header = lines[0].split()
    nx      = int(header[0])
    ny      = int(header[1])
    L       = float(header[2])
    delta_t = float(header[3])
    n_ites  = int(header[4])
    alpha = float(header[5])
    kappa = float(header[6])

    # ===== Lecture des blocs =====
    blocs = []
    current_constante = None
    current_data = []

    for i in range(1, len(lines)):
        line = lines[i].strip()
        if line == "":
            continue

        if ";" not in line:
            # Nouvelle constante → sauvegarder le bloc précédent
            if current_data:
                blocs.append({
                    'constante': current_constante,
                    'data': np.array(current_data)  # shape (n_ites, nx)
                })
                current_data = []
            current_constante = float(line)
        else:
            current_data.append([float(v) for v in line.split(";")])

    # Dernier bloc
    if current_data:
        blocs.append({
            'constante': current_constante,
            'data': np.array(current_data)
        })

    return nx, ny, L, delta_t, n_ites,alpha, kappa, blocs


def c_theorique_diffusion_pure(y, t, dist):
    return 1/2 * (1-math.erf((y-dist/2)/(2*np.sqrt(kappa*t))))


def calc_t95(bloc, delta_t):
        cmax       = bloc['data'][0]
        t          = np.arange(len(cmax)) * delta_t
        cmax_final = cmax[-1]
        variation  = np.abs(cmax - cmax_final) / (cmax_final if cmax_final != 0 else 1.)
        idx = np.where(variation > 0.05)[0]
        return t[idx[-1]] if len(idx) > 0 else t[0]


if mode == "advection_pure_verticale":
    nb_courbes = 5
    
    # ===== Lecture du fichier =====
    filename = "data_advection_pure_verticale_python.dat"

    with open(filename, "r") as f:
        lines = f.readlines()

    # première ligne : paramètres
    header = lines[0].split()
    nx = int(header[0])
    ny = int(header[1])
    L = float(header[2])
    delta_t, n_ites = float(header[3]), int(header[4])

    # ===== Lecture des données =====
    data = []
    for line in lines[1:]:
        values = np.array([float(v) for v in line.strip().split(";")])
        data.append(values)

    data = np.array(data)

    # ===== Axe y =====
    y = np.linspace(0, 2*L, nx)*1000

    # ===== Plot =====
    plt.figure(figsize=(8,5))

    for i in range(0, nb_courbes):
        plt.plot(y, data[int(i*n_ites/nb_courbes)], label=f"C(t={i*n_ites/nb_courbes*delta_t*1000:.2f}ms)")
    plt.xlabel("x (mm)")
    plt.ylabel("c(x, y = L/2)")
    plt.title("Evolution du profil de concentration en y = L/2 pour différents instants")
    plt.legend()
    plt.grid()
    plt.savefig("advection_pure_verticale.png", dpi=300)


if mode == "advection_pure_horizontale":
    nb_courbes = 5

    # ===== Lecture du fichier =====
    filename = "data_advection_pure_horizontale_python.dat"

    with open(filename, "r") as f:
        lines = f.readlines()

    # première ligne : paramètres
    header = lines[0].split()
    nx = int(header[0])
    ny = int(header[1])
    L = float(header[2])
    delta_t, n_ites = float(header[3]), int(header[4])

    # ===== Lecture des données =====
    data = []
    for line in lines[1:]:
        values = np.array([float(v) for v in line.strip().split(";")])
        data.append(values)

    data = np.array(data)

    # ===== Axe y =====
    y = np.linspace(0, L, ny)*1000

    # ===== Plot =====
    plt.figure(figsize=(8,5))

    for i in range(0, nb_courbes):
        plt.plot(y, data[int(i*n_ites/nb_courbes)], label=f"C(t={i*n_ites/nb_courbes*delta_t*1000:.2f}ms)")
    plt.xlabel("x (mm)")
    plt.ylabel("c(x=L, y)")
    plt.title("Evolution du profil de concentration en x = L")
    plt.legend()
    plt.grid()
    plt.savefig("advection_pure_horizontale.png", dpi=300)


if mode == "advection_pure_verticale_CLF":
    nb_blocks = 5
    # ===== Lecture du fichier =====
    filename = "data_advection_pure_verticale_CLF_python.dat"

    nx, ny, L, delta_t, n_ites, alpha, kappa,  blocs = read_multi_constante_file(filename)
    t = 10*delta_t*1000 #en ms
    # ===== Axe y =====
    y = np.linspace(0, 2*L, nx)*1000

    # ===== Plot =====
    plt.figure(figsize=(8,5))

    for i in range(0, nb_blocks):
        plt.plot(y, blocs[i]['data'][10, :], label=f"CLF = {blocs[i]['constante']:.2f}")
    plt.xlabel("x (mm)")
    plt.ylabel(f"c(x, y = L/2, t = {t:.2f}ms)")
    #plt.title("Evolution du profil de concentration en y = L/2")
    plt.legend()
    plt.grid()
    plt.savefig("advection_pure_verticale_CLF.png", dpi=300)


if mode == "diffusion_pure_horizontale":
    nb_courbes = 3

    # ===== Lecture du fichier =====
    filename = "data_diffusion_pure_horizontale_python.dat"

    with open(filename, "r") as f:
        lines = f.readlines()

    # première ligne : paramètres
    header = lines[0].split()
    nx = int(header[0])
    ny = int(header[1])
    L = float(header[2])
    delta_t, n_ites = float(header[3]), int(header[4])
    kappa = float(header[5])


    # ===== Lecture des données =====
    data = []
    for line in lines[1:]:
        values = np.array([float(v) for v in line.strip().split(";")])
        data.append(values)

    data = np.array(data)

    # ===== Axe y =====
    y = np.linspace(0, L, ny)*1000

    # ===== Plot =====
    plt.figure(figsize=(8,5))
    for i in range(nb_courbes):
        t = i * n_ites / nb_courbes * delta_t
        # Courbe expérimentale
        line, = plt.plot(y, data[int(i * n_ites / nb_courbes)], label=f"C(t={t*1000:.2f}ms)")
        if i > 0:
            ci = np.array([c_theorique_diffusion_pure(y[j]/1000, t, L) for j in range(ny)])
            plt.plot(y, ci, '--', color=line.get_color(), label=f"C_th(t={t*1000:.2f}ms)")
    plt.xlabel("x (mm)")
    plt.ylabel("c(x=L, y)")
    plt.title("Evolution du profil de concentration en x = L")
    plt.legend()
    plt.grid()
    plt.savefig("diffusion_pure_horizontale.png", dpi=300)


if mode == "diffusion_pure_horizontale_R":
    nb_blocks = 5
    # ===== Lecture du fichier =====
    filename = "data_diffusion_pure_horizontale_R_python.dat"

    nx, ny, L, delta_t, n_ites, alpha, kappa, blocs = read_multi_constante_file(filename)

    # ===== Axe y =====
    y = np.linspace(0, L, ny)*1000

    # ===== Plot =====
    plt.figure(figsize=(8,5))

    for i in range(0, nb_blocks):
        plt.plot(y, blocs[i]['data'][10, :], label=f"R = {blocs[i]['constante']:.2f}")
    plt.xlabel("x (mm)")
    plt.ylabel("c(x = L, y)")
    plt.title("Evolution du profil de concentration en x = L")
    plt.legend()
    plt.grid()
    plt.savefig("diffusion_pure_horizontale_R.png", dpi=300)


if mode == "diffusion_pure_verticale":
    nb_courbes = 3

    # ===== Lecture du fichier =====
    filename = "data_diffusion_pure_verticale_python.dat"

    with open(filename, "r") as f:
        lines = f.readlines()

    # première ligne : paramètres
    header = lines[0].split()
    nx = int(header[0])
    ny = int(header[1])
    L = float(header[2])
    delta_t, n_ites = float(header[3]), int(header[4])
    kappa = float(header[5])


    # ===== Lecture des données =====
    data = []
    for line in lines[1:]:
        values = np.array([float(v) for v in line.strip().split(";")])
        data.append(values)

    data = np.array(data)

    # ===== Axe y =====
    y = np.linspace(0, 2*L, nx)*1000

    # ===== Plot =====
    plt.figure(figsize=(8,5))
    for i in range(nb_courbes):
        t = i * n_ites / nb_courbes * delta_t
        # Courbe expérimentale
        line, = plt.plot(y, data[int(i * n_ites / nb_courbes)], label=f"C(t={t*1000:.2f}ms)")
        if i > 0:
            ci = np.array([c_theorique_diffusion_pure(y[j]/1000, t, 2*L) for j in range(nx)])
            plt.plot(y, ci, '--', color=line.get_color(), label=f"C_th(t={t*1000:.2f}ms)")
    plt.xlabel("x (mm)")
    plt.ylabel("c(x, y=L/2)")
    plt.title("Comparaison des profils de concentration en y = L/2")
    plt.legend()
    plt.grid()
    plt.savefig("diffusion_pure_verticale.png", dpi=300)


if mode == "diffusion_pure_verticale_R":
    nb_blocks = 5
    # ===== Lecture du fichier =====
    filename = "data_diffusion_pure_verticale_R_python.dat"

    nx, ny, L, delta_t, n_ites, alpha, kappa, blocs = read_multi_constante_file(filename)

    # ===== Axe y =====
    y = np.linspace(0, 2*L, nx)*1000

    # ===== Plot =====
    plt.figure(figsize=(8,5))

    for i in range(0, nb_blocks):
        plt.plot(y, blocs[i]['data'][10, :], label=f"R = {blocs[i]['constante']:.2f}")
    plt.xlabel("x (mm)")
    plt.ylabel("c(x , y=L/2)")
    plt.title("Evolution du profil de concentration en y = L/2")
    plt.legend()
    plt.grid()
    plt.savefig("diffusion_pure_verticale_R.png", dpi=300)


if mode == "convergence_maillage":
    nb_blocks = 6
    # ===== Lecture du fichier =====
    filename = "data_convergence_maillage_python.dat"

    nx, ny, L, delta_t, n_ites, alpha, kappa, blocs = read_multi_constante_file(filename)

    # ===== Plot =====
    plt.figure(figsize=(8,5))

    R = 0.5
    tf = 0.05e-3
    for i in range(0, nb_blocks):
        nx_i = blocs[i]['data'].shape[1]  # nx propre à ce bloc
        ny_i = int(blocs[i]['constante']/nx_i)
        dx_f  = 2.0 * L / nx_i
        dy_f  = L / ny_i
        dt_i    = 1.0 / (alpha/dx_f + alpha/dy_f
                          + kappa/(R*dx_f**2) + kappa/(R*dy_f**2))
        y_i = np.linspace(0, 2*L, nx_i) * 1000
        plt.plot(y_i, blocs[i]['data'][int(tf/dt_i)-1, :], label=f"nx, ny = {nx_i},{ny_i}")
    plt.xlabel("x (mm)")
    plt.ylabel(f"c(x , y=L/2, t={tf*1000:.2f}ms)")
    #plt.title("Evolution du profil de concentration en y = L/2")
    plt.legend()
    plt.grid()
    plt.savefig("convergence_maillage.png", dpi=300)


if mode == "peclet_diffusion":
    # ===== Lecture du fichier =====
    filename = "data_peclet_python.dat"
 
    nx, ny, L, delta_t, n_ites, alpha, kappa, blocs = read_multi_constante_file(filename)

    dx_f  = 2.0 * L / nx
    dy_f  = L / ny
    CFL_f = 1.0
    R_f   = 0.5
    Pe_diff    = []
    t95_diff   = []
    tau_d_diff = []
 
    for bloc in blocs:
        Pe      = bloc['constante']
        kappa_i = alpha * L / Pe
        dt_i    = 1.0 / (alpha/dx_f + alpha/dy_f
                          + kappa_i/(R_f*dx_f**2) + kappa_i/(R_f*dy_f**2))
        Pe_diff.append(Pe)
        t95_diff.append(calc_t95(bloc, dt_i))
        tau_d_diff.append(L**2 / kappa_i)

    Pe_diff    = np.array(Pe_diff)
    t95_diff   = np.array(t95_diff)
 
    # ===== Plot =====
    plt.figure(figsize=(8,5))
 
    for bloc in blocs:
        dt_i    = 1.0 / (alpha/dx_f + alpha/dy_f
                          + kappa_i/(R_f*dx_f**2) + kappa_i/(R_f*dy_f**2))
        Pe   = bloc['constante']
        cmax = bloc['data'][0]
        t    = np.arange(len(cmax)) * dt_i
        plt.plot(t, cmax, label=f"Pe = $10^{{{np.log10(Pe):.0f}}}$")
    plt.xlabel("t (s)")
    plt.ylabel("C_max")
    plt.title("Évolution des profils de concentration maximale")
    plt.legend()
    plt.grid()
    plt.savefig("peclet_diffusion.png", dpi=300)
 
    # ===== Plot t95% vs Pe =====
    plt.figure(figsize=(8, 5))
    plt.plot(Pe_diff, t95_diff, 'o-')
    plt.xscale('log')
    plt.xlabel("Pe")
    plt.ylabel("$t_{95\%}$ (s)")
    plt.title("Temps de convergence à 95% en fonction du nombre de Péclet")
    plt.grid()
    plt.savefig("t95_vs_peclet_diffusion.png", dpi=300)
        
 
if mode == "peclet_advection":
    
    # ===== Lecture du fichier =====
    filename = "data_peclet_python.dat"
 
    nx, ny, L, delta_t, n_ites, alpha, kappa, blocs = read_multi_constante_file(filename)

    dx_f  = 2.0 * L / nx
    dy_f  = L / ny
    CFL_f = 1.0
    R_f   = 0.5
    Pe_adv    = []
    t95_adv   = []
    tau_c_adv = []
 
    for bloc in blocs:
        Pe      = bloc['constante']
        alpha_i = kappa * Pe / L
        dt_i    = 1.0 / (alpha_i/dx_f + alpha_i/dy_f
                          + kappa/(R_f*dx_f**2) + kappa/(R_f*dy_f**2))
        Pe_adv.append(Pe)
        t95_adv.append(calc_t95(bloc, dt_i))
        tau_c_adv.append(L / alpha_i)

    Pe_adv    = np.array(Pe_adv)
    t95_adv   = np.array(t95_adv)
    tau_c_adv = np.array(tau_c_adv)
 
    # ===== Plot =====
    plt.figure(figsize=(8,5))
 
    for bloc in blocs:
        Pe   = bloc['constante']
        cmax = bloc['data'][0]
        t    = np.arange(len(cmax)) * delta_t
        plt.plot(t, cmax, label=f"Pe = $10^{{{np.log10(Pe):.0f}}}$")
    plt.xlabel("t (s)")
    plt.ylabel("C_max")
    plt.title("Évolution des profils de concentration maximale")
    plt.legend()
    plt.grid()
    plt.savefig("peclet_advection.png", dpi=300)
 
    # ===== Plot t95% vs Pe =====
    plt.figure(figsize=(8, 5))
    plt.plot(Pe_adv, t95_adv, 'o-')
    plt.xscale('log')
    plt.xlabel("Pe")
    plt.ylabel("$t_{95\%}$ (s)")
    plt.title("Temps de convergence à 95% en fonction du nombre de Péclet")
    plt.grid()
    plt.savefig("t95_vs_peclet_advection.png", dpi=300)
 
 
if mode == "peclet_finale":
    # ===== Lecture des deux fichiers =====
    # data_peclet_python.dat     : boucle diffusion (kappa variable, alpha fixe)
    # data_peclet_adv_python.dat : boucle advection  (alpha variable, kappa fixe)
 
    nx, ny, L, delta_t, n_ites, alpha_header, kappa_header, blocs_diff = \
        read_multi_constante_file("data_peclet_python.dat")
 
    _, _, _, _, _, _, _, blocs_adv = \
        read_multi_constante_file("data_peclet_adv_python.dat")
 
    if len(blocs_diff) != 9:
        print(f"Attention : {len(blocs_diff)} blocs dans data_peclet_python.dat, 9 attendus.")
    if len(blocs_adv) != 9:
        print(f"Attention : {len(blocs_adv)} blocs dans data_peclet_adv_python.dat, 9 attendus.")
 
    dx_f  = 2.0 * L / nx
    dy_f  = L / ny
    CFL_f = 1.0
    R_f   = 0.5
 
    # ---- Série diffusion : alpha fixe = alpha_header, kappa = alpha*L/Pe ----
    Pe_diff    = []
    t95_diff   = []
    tau_d_diff = []
 
    for bloc in blocs_diff:
        Pe      = bloc['constante']
        kappa_i = alpha_header * L / Pe
        dt_i    = 1.0 / (alpha_header/dx_f + alpha_header/dy_f
                          + kappa_i/(R_f*dx_f**2) + kappa_i/(R_f*dy_f**2))
        Pe_diff.append(Pe)
        t95_diff.append(calc_t95(bloc, dt_i))
        tau_d_diff.append(L**2 / kappa_i)
 
    # ---- Série advection : kappa fixe = kappa_header, alpha = kappa*Pe/L ----
    Pe_adv    = []
    t95_adv   = []
    tau_c_adv = []
 
    for bloc in blocs_adv:
        Pe      = bloc['constante']
        alpha_i = kappa_header * Pe / L
        dt_i    = 1.0 / (alpha_i/dx_f + alpha_i/dy_f
                          + kappa_header/(R_f*dx_f**2) + kappa_header/(R_f*dy_f**2))
        Pe_adv.append(Pe)
        t95_adv.append(calc_t95(bloc, dt_i))
        tau_c_adv.append(L / alpha_i)
 
    Pe_diff    = np.array(Pe_diff)
    t95_diff   = np.array(t95_diff)
    tau_d_diff = np.array(tau_d_diff)
 
    Pe_adv    = np.array(Pe_adv)
    t95_adv   = np.array(t95_adv)
    tau_c_adv = np.array(tau_c_adv)
 
    # ===== Graphique final =====
    plt.figure(figsize=(9, 6))
    plt.loglog(Pe_diff, t95_diff / tau_d_diff, 'o-', label=r"$t_{95\%}/\tau_d$  (κ variable, α fixe)")
    plt.loglog(Pe_adv,  t95_adv  / tau_c_adv,  's-', label=r"$t_{95\%}/\tau_c$  (α variable, κ fixe)")
    plt.xlabel("Pe")
    plt.ylabel("Temps adimensionnel")
    plt.title(r"Temps adimensionnels $t_{95\%}/\tau_d$ et $t_{95\%}/\tau_c$ en fonction de Pe")
    plt.legend()
    plt.grid(which='both')
    plt.savefig("t95_adim_vs_peclet_finale.png", dpi=300)