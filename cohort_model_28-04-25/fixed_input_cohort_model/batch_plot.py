import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
from concurrent.futures import ProcessPoolExecutor

files = [
    ('Initial_SOC.csv', 'Initial SOC'),
    ('absolute_SOC_2050.csv', 'Absolute SOC 2050'),
    ('absolute_SOC_2080.csv', 'Absolute SOC 2080'),
    ('delta_SOC_2050.csv', 'Delta SOC 2050'),
    ('delta_SOC_2080.csv', 'Delta SOC 2080'),
    ('tree_carbon_2050.csv', 'Tree Carbon 2050'),
    ('tree_carbon_2080.csv', 'Tree Carbon 2080'),
    ('cumulative_tree_carbon_2050.csv', 'Cumulative Tree Carbon 2050'),
    ('cumulative_tree_carbon_2080.csv', 'Cumulative Tree Carbon 2080'),
    ('delta_biome_carbon_2050.csv', 'Delta Biome Carbon 2050'),
    ('delta_biome_carbon_2080.csv', 'Delta Biome Carbon 2080'),
]

def plot_one(args):
    fname, title = args
    data = np.loadtxt(fname, delimiter=',')
    x, y, vals = data[:,0], data[:,1], data[:,2]
    sf = 1000.0
    rows = 1 + int((y.max()-y.min())/sf)
    cols = 1 + int((x.max()-x.min())/sf)
    grid = np.full((rows, cols), np.nan)
    yi = ((y - y.min())/sf).astype(int)
    xi = ((x - x.min())/sf).astype(int)
    np.clip(yi, 0, rows-1, out=yi)
    np.clip(xi, 0, cols-1, out=xi)
    grid[yi, xi] = vals
    fig, ax = plt.subplots()
    ax.set_axis_off()
    img = ax.imshow(np.ma.masked_invalid(grid), origin='lower', interpolation='nearest', cmap='gist_ncar')
    cbar = plt.colorbar(img, shrink=0.5)
    ax.set_title(title)
    cbar.set_label('SOC (t C ha$^{-1}$)', rotation=270, labelpad=15)
    ax.xaxis.set_major_formatter(FuncFormatter(lambda x, _: f'{int(x)}'))
    ax.yaxis.set_major_formatter(FuncFormatter(lambda y, _: f'{int(y)}'))
    out = title.replace(' ', '_') + '.png'
    fig.savefig(out, dpi=500)
    plt.close(fig)
    return out

if __name__ == '__main__':
    with ProcessPoolExecutor() as executor:
        for output_path in executor.map(plot_one, files):
            print(f'Wrote {output_path}')
