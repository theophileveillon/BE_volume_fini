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

    for i in range(0, nb_blocks):
        nx_i = blocs[i]['data'].shape[1]  # nx propre à ce bloc
        y_i = np.linspace(0, 2*L, nx_i) * 1000
        plt.plot(y_i, blocs[i]['data'][n_ites-1, :], label=f"nx, ny = {int(blocs[i]['constante']/nx_i)}, {nx_i}")
    plt.xlabel("x (mm)")
    plt.ylabel(f"c(x , y=L/2, t={n_ites*delta_t*1000:.2f}ms)")
    #plt.title("Evolution du profil de concentration en y = L/2")
    plt.legend()
    plt.grid()
    plt.savefig("convergence_maillage.png", dpi=300)


if mode == "peclet_diffusion":
    # ===== Lecture du fichier =====
    filename = "data_peclet_python.dat"

    nx, ny, L, delta_t, n_ites, alpha, kappa, blocs = read_multi_constante_file(filename)

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
    plt.savefig("peclet_diffusion.png", dpi=300)

    # ===== Calcul des t95% =====
    t95_list = []
    Pe_list  = []

    for bloc in blocs:
        Pe   = bloc['constante']
        cmax = bloc['data'][0]
        t    = np.arange(len(cmax)) * delta_t
        
        cmax_final = cmax[-1]
        variation  = np.abs(cmax - cmax_final) / cmax_final
        
        # On cherche le dernier indice où on dépasse 5%
        idx = np.where(variation > 0.05)[0]
        if len(idx) > 0:
            t95 = t[idx[-1]]
        else:
            t95 = t[0]
        
        Pe_list.append(Pe)
        t95_list.append(t95)

    # ===== Plot t95% vs Pe =====
    plt.figure(figsize=(8, 5))
    plt.plot(Pe_list, t95_list, 'o-')
    plt.xscale('log')
    plt.xlabel("Pe")
    plt.ylabel("$t_{95\%}$ (s)")
    plt.title("Temps de convergence à 95% en fonction du nombre de Péclet")
    plt.grid()
    plt.savefig("t95_vs_peclet_diffusion.png", dpi=300)

    # ===== Calcul des temps caractéristiques =====
    t95_list = []
    Pe_list  = []

    for bloc in blocs:
        Pe   = bloc['constante']
        alpha = kappa * Pe / L
        tau_d = L**2 / kappa
        tau_c = L / alpha
        cmax = bloc['data'][0]
        t    = np.arange(len(cmax)) * delta_t
        
        cmax_final = cmax[-1]
        variation  = np.abs(cmax - cmax_final) / cmax_final
        
        idx = np.where(variation > 0.05)[0]
        if len(idx) > 0:
            t95 = t[idx[-1]]
        else:
            t95 = t[0]
        
        Pe_list.append(Pe)
        t95_list.append(t95)

    t95_arr = np.array(t95_list)
    Pe_arr  = np.array(Pe_list)

    # ===== Plot t95*/τ vs Pe =====
    plt.figure(figsize=(8, 5))
    plt.loglog(Pe_arr, t95_arr / tau_d, 'o-', label=r"$t_{95\%}/\tau_d$")
    plt.loglog(Pe_arr, t95_arr / tau_c, 's-', label=r"$t_{95\%}/\tau_c$")
    plt.xlabel("Pe")
    plt.ylabel("Temps adimensionnel")
    plt.title(r"Temps adimensionnels $t_{95\%}/\tau_d$ et $t_{95\%}/\tau_c$ en fonction de Pe")
    plt.legend()
    plt.grid(which='both')
    plt.savefig("t95_adim_vs_peclet_diffusion.png", dpi=300)
        

if mode == "peclet_advection":
    
    # ===== Lecture du fichier =====
    filename = "data_peclet_python.dat"

    nx, ny, L, delta_t, n_ites, alpha, kappa, blocs = read_multi_constante_file(filename)

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

    # ===== Calcul des t95% =====
    t95_list = []
    Pe_list  = []

    for bloc in blocs:
        Pe   = bloc['constante']
        cmax = bloc['data'][0]
        t    = np.arange(len(cmax)) * delta_t
        
        cmax_final = cmax[-1]
        variation  = np.abs(cmax - cmax_final) / cmax_final
        
        # On cherche le dernier indice où on dépasse 5%
        idx = np.where(variation > 0.05)[0]
        if len(idx) > 0:
            t95 = t[idx[-1]]
        else:
            t95 = t[0]
        
        Pe_list.append(Pe)
        t95_list.append(t95)

    # ===== Plot t95% vs Pe =====
    plt.figure(figsize=(8, 5))
    plt.plot(Pe_list, t95_list, 'o-')
    plt.xscale('log')
    plt.xlabel("Pe")
    plt.ylabel("$t_{95\%}$ (s)")
    plt.title("Temps de convergence à 95% en fonction du nombre de Péclet")
    plt.grid()
    plt.savefig("t95_vs_peclet_advection.png", dpi=300)

    # ===== Calcul des temps caractéristiques =====
    t95_list = []
    Pe_list  = []

    for bloc in blocs:
        Pe   = bloc['constante']
        alpha = kappa * Pe / L
        tau_d = L**2 / kappa
        tau_c = L / alpha
        cmax = bloc['data'][0]
        t    = np.arange(len(cmax)) * delta_t
        
        cmax_final = cmax[-1]
        variation  = np.abs(cmax - cmax_final) / cmax_final
        
        idx = np.where(variation > 0.05)[0]
        if len(idx) > 0:
            t95 = t[idx[-1]]
        else:
            t95 = t[0]
        
        Pe_list.append(Pe)
        t95_list.append(t95)

    t95_arr = np.array(t95_list)
    Pe_arr  = np.array(Pe_list)

    # ===== Plot t95*/τ vs Pe =====
    plt.figure(figsize=(8, 5))
    plt.loglog(Pe_arr, t95_arr / tau_d, 'o-', label=r"$t_{95\%}/\tau_d$")
    plt.loglog(Pe_arr, t95_arr / tau_c, 's-', label=r"$t_{95\%}/\tau_c$")
    plt.xlabel("Pe")
    plt.ylabel("Temps adimensionnel")
    plt.title(r"Temps adimensionnels $t_{95\%}/\tau_d$ et $t_{95\%}/\tau_c$ en fonction de Pe")
    plt.legend()
    plt.grid(which='both')
    plt.savefig("t95_adim_vs_peclet_advection.png", dpi=300)
