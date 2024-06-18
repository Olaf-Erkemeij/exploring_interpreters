import matplotlib.pyplot as plt
import matplotlib
import numpy as np
import readline
import pandas as pd

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
    data = pd.read_csv("mini-java/data.csv")

    N = data["N"]
    Size = data["Size"]

    plt.plot(N, Size, label="Size of the storage in bytes")

    plt.xlabel("Nodes")
    plt.ylabel("State storage size (bytes)")

    plt.gca().yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(formatBytes))

    # plt.tight_layout()
    plt.title("State storage size for different tree sizes (p = 0.5)")

    plt.savefig("plot1.png", format="png", dpi=300)


def plotLargerTest():
    data = pd.read_csv("mini-java/data2.csv")

    N = data["N"]
    Size = data["Size"]

    plt.plot(N, Size, label="Size of the storage in bytes")

    plt.xlabel("Nodes")
    plt.ylabel("State storage size (bytes)")

    plt.gca().yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(formatBytes))

    # plt.tight_layout()
    plt.title("State storage size for different tree sizes (p = 0.5)")

    plt.savefig("plot2.png", format="png", dpi=300)


if __name__ == "__main__":
    # plotLargerTest()
    plotSmallTest()