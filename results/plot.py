import matplotlib.pyplot as plt
import matplotlib
import numpy as np
import readline
import pandas as pd
from scipy.interpolate import interp1d

# matplotlib.use("pgf")
# matplotlib.rcParams.update({
#     "pgf.texsystem": "pdflatex",
#     'font.family': 'sans-serif',
#     'font.size': 20,
#     'text.usetex': True,
#     'pgf.rcfonts': False,
#     'savefig.transparent': True,
#     'savefig.bbox': 'tight',
#     'savefig.dpi': 300,
# })

def formatBytes(x, pos):
    if x < 0: return ""

    for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
        if x < 1024.0:
            return f"{x:3.1f} {unit}"
        x /= 1024.0


def plotSmallTest():
    data = pd.read_csv("data/data3.csv")

    N = data["N"]
    Size = data["Size"]

    plt.plot(N, Size, label="Size of the storage in bytes")

    plt.xlabel("Nodes")
    plt.ylabel("State storage size (bytes)")

    plt.gca().yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(formatBytes))

    # plt.tight_layout()
    plt.title("State storage size for different tree sizes (p = 0.5)")

    plt.savefig("plot4.png", format="png", dpi=300)


def plotLargerTest():
    data = pd.read_csv("data/data4.csv")

    N = data["N"]
    Size = data["Size"]

    plt.plot(N, Size, label="Size of the storage in bytes")

    plt.xlabel("Nodes")
    plt.ylabel("State storage size (bytes)")

    plt.gca().yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(formatBytes))

    # plt.tight_layout()
    plt.title("State storage size for different tree sizes (p = 0.5)")

    plt.savefig("plot2.png", format="png", dpi=300)


def plotProbTest():
    # Load and prepare data
    plt.figure(figsize=(10, 6))
    data = pd.read_csv("data/data3.csv")
    data['size'] = data['Cmap'] + data['ExecEnv']

    # Aggregate data by N and P
    agg_data = data.groupby(['N', 'P']).agg(
        mean_size=('size', 'mean'),
        std_size=('size', 'std')
    ).reset_index()

    colors = plt.cm.viridis(np.linspace(0, 1, 11))

    for i, p in enumerate(np.linspace(0, 1, 11)):
        p = round(p, 1)
        data_p = agg_data[agg_data['P'] == p].sort_values('N')

        N = data_p['N'].values
        mean_size = data_p['mean_size'].values
        std_size = data_p['std_size'].values

        # Fit quadratic polynomial to mean size
        coeffs = np.polyfit(N, mean_size, 2)
        poly = np.poly1d(coeffs)
        x_smooth = np.linspace(N.min(), N.max(), 300)
        y_smooth = poly(x_smooth)

        # Calculate SEM
        sem = std_size / np.sqrt(5)
        interp_sem = interp1d(N, sem, kind='linear', fill_value='extrapolate')
        sem_smooth = interp_sem(x_smooth)

        color = colors[i]
        label = f'P = {p:.1f}'

        plt.plot(x_smooth, y_smooth, color=color, label=label)
        plt.fill_between(x_smooth, y_smooth - sem_smooth, y_smooth + sem_smooth,
                         color=color, alpha=0.2)

    plt.xlabel("Number of Nodes (N)", fontsize=12)
    plt.ylabel("State Storage Size", fontsize=12)
    plt.gca().yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(formatBytes))

    # Add grid and adjust ticks
    plt.grid(True, which='both', linestyle='--', alpha=0.3)
    plt.minorticks_on()

    # Set axis limits and padding
    plt.xlim(left=0)
    plt.ylim(bottom=0)
    plt.tight_layout(pad=2)

    plt.title("Memory usage vs. Nodes for different probabilities")
    plt.legend(title="Probability (P)", loc='upper left')
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    plt.savefig("plot_prob_test.png", dpi=300, bbox_inches='tight')
    plt.close()


def plot_detail_test():
    data = pd.read_csv("data/data3.csv")
    plt.figure(figsize=(10, 6))

    colors = {'0.0': '#1f77b4', '0.5': '#ff7f0e'} 
    components = {'Cmap': '-', 'ExecEnv': '--'}

    for p in [0.0, 0.5]:
        p_key = f"{p:.1f}"
        p_data = data[data["P"] == p]

        agg_data = p_data.groupby('N').agg(
            cmap_mean=('Cmap', 'mean'),
            cmap_std=('Cmap', 'std'),
            execenv_mean=('ExecEnv', 'mean'),
            execenv_std=('ExecEnv', 'std')
        ).reset_index()

        for component in ['Cmap', 'ExecEnv']:
            mean = agg_data[f"{component.lower()}_mean"]
            std = agg_data[f"{component.lower()}_std"]
            N = agg_data["N"]

            plt.plot(N, mean,
                    linestyle=components[component],
                    color=colors[p_key],
                    label=f'{component} (P={p})',
                    linewidth=2)

            plt.fill_between(N,
                            mean - std,
                            mean + std,
                            color=colors[p_key],
                            alpha=0.15)

    handles, labels = plt.gca().get_legend_handles_labels()
    unique_labels = dict(zip(labels, handles))  # Remove duplicates
    plt.legend(unique_labels.values(), unique_labels.keys(),
              title="Component & Probability",
              loc='upper left')

    plt.xlabel("Number of Nodes (N)", fontsize=12, labelpad=10)
    plt.ylabel("State Storage Size", fontsize=12, labelpad=10)
    plt.gca().yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(formatBytes))
    plt.title("Memory Usage Analysis: Component Breakdown by Tree Size and Probability",
             fontsize=14, pad=20)

    plt.grid(True, which='both', linestyle='--', alpha=0.3)
    plt.minorticks_on()

    plt.xlim(left=0)
    plt.ylim(bottom=0)
    plt.tight_layout(pad=2)

    plt.savefig("plot_detail_test.png", dpi=300, bbox_inches='tight')
    plt.close()


def plot_larger_test():
    plt.figure(figsize=(10, 6))
    data = pd.read_csv("data/data4.csv")
    data['size'] = data['Cmap'] + data['ExecEnv']

    agg_data = data.groupby(['N', 'P']).agg(
        mean_size=('size', 'mean'),
        std_size=('size', 'std')
    ).reset_index()

    colors = plt.cm.viridis(np.linspace(0, 1, 11))

    for i, p in enumerate(np.linspace(0, 1, 11)):
        p = round(p, 1)
        data_p = agg_data[agg_data['P'] == p].sort_values('N')

        N = data_p['N'].values
        mean_size = data_p['mean_size'].values
        std_size = data_p['std_size'].values

        coeffs = np.polyfit(N, mean_size, 2)
        poly = np.poly1d(coeffs)
        x_smooth = np.linspace(N.min(), N.max(), 300)
        y_smooth = poly(x_smooth)

        # Calculate SEM
        sem = std_size / np.sqrt(5)
        interp_sem = interp1d(N, sem, kind='linear', fill_value='extrapolate')
        sem_smooth = interp_sem(x_smooth)

        color = colors[i]
        label = f'P = {p:.1f}'

        plt.plot(x_smooth, y_smooth, color=color, label=label)
        plt.fill_between(x_smooth, y_smooth - sem_smooth, y_smooth + sem_smooth,
                         color=color, alpha=0.2)

    plt.xlabel("Number of Nodes (N)", fontsize=12)
    plt.ylabel("State Storage Size", fontsize=12)
    plt.gca().yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(formatBytes))

    # Add grid and adjust ticks
    plt.grid(True, which='both', linestyle='--', alpha=0.3)
    plt.minorticks_on()

    # Set axis limits and padding
    # plt.xlim(left=100)
    # plt.ylim(bottom=0)
    plt.tight_layout(pad=2)

    plt.title("Memory usage vs. Nodes for different probabilities")
    plt.legend(title="Probability (P)", loc='upper left')
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    plt.savefig("plot_larger_test.png", dpi=300, bbox_inches='tight')
    plt.close()

def plot_monadic_test():
    for test in ["monadic1", "monadic3"]:
        data = pd.read_csv(f"data/data_{test}.csv")
        data['size'] = data['Cmap'] + data['ExecEnv']

        agg_data = data.groupby(['N', 'P']).agg(
            mean_size=('size', 'mean'),
            std_size=('size', 'std')
        ).reset_index()

        for i, p in enumerate(np.linspace(0, 1, 11)):
            p = round(p, 1)
            if p > 0.1: continue

            data_p = agg_data[agg_data['P'] == p].sort_values('N')

            N = data_p['N'].values
            mean_size = data_p['mean_size'].values
            std_size = data_p['std_size'].values

            coeffs = np.polyfit(N, mean_size, 2)
            poly = np.poly1d(coeffs)
            x_smooth = np.linspace(N.min(), N.max(), 300)
            y_smooth = poly(x_smooth)

            # Calculate SEM
            sem = std_size / np.sqrt(5)
            interp_sem = interp1d(N, sem, kind='linear', fill_value='extrapolate')
            sem_smooth = interp_sem(x_smooth)

            label = f'P = {p:.1f} (Test {test[-1]})'

            plt.plot(x_smooth, y_smooth, label=label)
            plt.fill_between(x_smooth, y_smooth - sem_smooth, y_smooth + sem_smooth, alpha=0.2)
        
    plt.xlabel("Number of Nodes (N)", fontsize=12)
    plt.ylabel("State Storage Size", fontsize=12)
    plt.gca().yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(formatBytes))

    # Add grid and adjust ticks
    plt.grid(True, which='both', linestyle='--', alpha=0.3)
    plt.minorticks_on()

    plt.title("Memory usage vs. Nodes for different probabilities")
    plt.legend(title="Probability (P)", loc='upper left')
    plt.grid(True, alpha=0.3)
    plt.tight_layout()

    plt.savefig(f"plot_monadic.png", dpi=300, bbox_inches='tight')
    plt.close()

if __name__ == "__main__":
    plot_monadic_test()
    # plotLargerTest()
    # plotSmallTest()
    # plotProbTest()
    # plotDetailTest()
    # plot_detail_test()
    # plot_larger_test()