import matplotlib.pyplot as plt
import matplotlib
import numpy as np
import pandas as pd
from scipy.interpolate import interp1d
import seaborn as sns


def set_pgf():
    matplotlib.use("pgf")
    matplotlib.rcParams.update(
        {
            "pgf.texsystem": "pdflatex",
            "font.family": "sans-serif",
            "text.usetex": True,
            "axes.titlesize": 12,
            "font.size": 11,
            "axes.labelsize": 11,
            "xtick.labelsize": 10,
            "ytick.labelsize": 10,
            "legend.fontsize": 9,
            "legend.title_fontsize": 10,
            "pgf.rcfonts": False,
            "savefig.transparent": True,
            "savefig.bbox": "tight",
            "savefig.dpi": 300,
        }
    )


def formatBytes(x, pos):
    if x < 0:
        return ""

    for unit in ["B", "KB", "MB", "GB", "TB"]:
        if x < 1024.0:
            return f"{x:3.1f} {unit}"
        x /= 1024.0


def shared_legend(folder="pgf"):
    num_entries = 11
    colors = plt.cm.viridis(np.linspace(0, 1, num_entries))
    labels = [f"P = {p:.1f}" for p in np.linspace(0, 1, num_entries)]

    handles = [plt.Line2D([0], [0], color=colors[i], lw=2) for i in range(num_entries)]

    fig = plt.figure(figsize=(3.5, 2.5))

    fig.legend(
        handles, labels, title="Probability (P)", loc="center", frameon=False, ncol=3
    )

    plt.gca().set_axis_off()

    fig.savefig(
        f"{folder}/legend.pgf",
        bbox_inches="tight",
        pad_inches=0.02,
        transparent=True,
        dpi=300,
    )

    plt.close()


def plot_final_test(pgf=False):
    size_dict = {
        1: ["Cmap", "ExecEnv"],
        2: ["Cmap", "Parents", "Children"],
        3: ["Cmap", "Parents", "Children"],
        4: ["Cmap", "ExecEnv"],
        5: ["History"],
        6: ["Cmap", "ExecEnv"],
        7: ["Filesize"],
    }

    ext = "pgf" if pgf else "png"

    for folder in ["mini-java", "scheme"]:
        for version in range(1, 8):
            if pgf:
                plt.figure(figsize=(3.5, 2.5))
            else:
                plt.figure(figsize=(10, 5.5))

            data = pd.read_csv(f"data/{folder}/v{version}.csv")
            data["size"] = sum([data[col] for col in size_dict[version]])

            agg_data = (
                data.groupby(["N", "P"])
                .agg(mean_size=("size", "mean"), std_size=("size", "std"))
                .reset_index()
            )

            colors = plt.cm.viridis(np.linspace(0, 1, 11))

            for i, p in enumerate(np.linspace(0, 1, 11)):
                p = round(p, 1)
                data_p = agg_data[agg_data["P"] == p].sort_values("N")

                N = data_p["N"].values
                mean_size = data_p["mean_size"].values
                std_size = data_p["std_size"].values

                coeffs = np.polyfit(N, mean_size, 2)
                poly = np.poly1d(coeffs)
                x_smooth = np.linspace(N.min(), N.max(), 300)
                y_smooth = poly(x_smooth)

                # Calculate SEM
                sem = std_size / np.sqrt(5)
                interp_sem = interp1d(N, sem, kind="linear", fill_value="extrapolate")
                sem_smooth = interp_sem(x_smooth)

                color = colors[i]
                label = f"P = {p:.1f}"

                plt.plot(x_smooth, y_smooth, color=color, label=label)
                plt.fill_between(
                    x_smooth,
                    y_smooth - sem_smooth,
                    y_smooth + sem_smooth,
                    color=color,
                    alpha=0.2,
                )

            plt.xlabel("Number of Nodes (N)")
            if pgf and version % 2:
                plt.ylabel("State Storage Size")
            plt.gca().yaxis.set_major_formatter(
                matplotlib.ticker.FuncFormatter(formatBytes)
            )

            # Add grid and adjust ticks
            plt.grid(True, which="both", linestyle="--", alpha=0.3)
            plt.minorticks_on()

            plt.grid(True, alpha=0.3)
            plt.tight_layout(pad=2)

            if not pgf:
                plt.legend(title="Probability (P)", loc="upper left")
            plt.savefig(f"{folder}/v{version}.{ext}", dpi=300)
            plt.close()


def plot_final_test2(pgf=False):
    size_dict = {
        1: ["Cmap", "ExecEnv"],
        2: ["Cmap", "Parents", "Children"],
        3: ["Cmap", "Parents", "Children"],
        4: ["Cmap", "ExecEnv"],
        5: ["History"],
        6: ["Cmap", "ExecEnv"],
        7: ["Filesize"],
    }

    order = {"scheme": [6, 2, 3, 4, 1, 5, 7], "mini-java": [6, 2, 3, 1, 5, 4, 7]}

    set_order = lambda f, v: [o for o in order[f] if o in v]

    plots = [
        ["all", [1, 2, 3, 4, 5, 6, 7]],
        ["all-6", [1, 2, 3, 4, 5, 7]],
        ["final", [1, 5, 7]],
    ]

    ext = "pgf" if pgf else "png"

    for folder in ["scheme", "mini-java"]:
        for name, versions in plots:
            plt.figure(figsize=(10, 5.5))

            for version in set_order(folder, versions):
                data = pd.read_csv(f"data/{folder}/v{version}_big.csv")
                data["size"] = sum([data[col] for col in size_dict[version]])

                agg_data = (
                    data.groupby(["N", "P"])
                    .agg(mean_size=("size", "mean"), std_size=("size", "std"))
                    .reset_index()
                )

                colors = sns.color_palette("tab10", 7)

                data_p = agg_data[agg_data["P"] == 0.5].sort_values("N")

                N = data_p["N"].values
                mean_size = data_p["mean_size"].values
                std_size = data_p["std_size"].values

                coeffs = np.polyfit(N, mean_size, 2)
                poly = np.poly1d(coeffs)
                x_smooth = np.linspace(N.min(), N.max(), 300)
                y_smooth = poly(x_smooth)

                # Calculate SEM
                sem = std_size / np.sqrt(5)
                interp_sem = interp1d(N, sem, kind="linear", fill_value="extrapolate")
                sem_smooth = interp_sem(x_smooth)

                color = colors[version - 1]
                label = f"Version = {version}"

                plt.plot(x_smooth, y_smooth, color=color, label=label, linestyle="--")
                plt.scatter(N, mean_size, color=color, marker="o", s=50)
                plt.fill_between(
                    x_smooth,
                    y_smooth - sem_smooth,
                    y_smooth + sem_smooth,
                    color=color,
                    alpha=0.2,
                )

            plt.xlabel("Number of Nodes (N)", fontsize=12)
            plt.ylabel("Total Memory Usage (P=0.5)", fontsize=12)
            plt.gca().yaxis.set_major_formatter(
                matplotlib.ticker.FuncFormatter(formatBytes)
            )

            # Add grid and adjust ticks
            plt.grid(True, which="both", linestyle="--", alpha=0.3)
            plt.minorticks_on()
            plt.xticks(np.arange(100, max(N) + 100, 100))

            plt.legend(title="Framework Version", loc="upper left")
            plt.grid(True, alpha=0.3)
            plt.tight_layout()
            plt.savefig(f"{folder}/{name}.{ext}", dpi=300)
            plt.close()

        if pgf:
            shared_legend(folder)


if __name__ == "__main__":
    plot_final_test()
    plot_final_test2()
    set_pgf()
    plot_final_test(pgf=True)
    plot_final_test2(pgf=True)
